//! Accessing components on entities.

use core::iter::FusedIterator;
use core::mem::{self, MaybeUninit};
use core::ptr::NonNull;
use core::{any, fmt};

use crate::archetype::{Archetype, ArchetypeIdx, ArchetypeRow, Archetypes};
use crate::assert::{assume_debug_checked, UnwrapDebugChecked};
use crate::entity::{Entities, EntityId, EntityLocation};
use crate::event::EventPtr;
use crate::handler::{HandlerConfig, HandlerInfo, HandlerParam, InitError};
use crate::query::{Query, ReadOnlyQuery};
use crate::sparse_map::SparseMap;
use crate::world::{UnsafeWorldCell, World};

/// Internal state for a [`Fetcher`].
#[doc(hidden)]
pub struct FetcherState<Q: Query> {
    map: SparseMap<ArchetypeIdx, Q::ArchState>,
    state: Q::State,
}

impl<Q: Query> FetcherState<Q> {
    pub(crate) fn new(state: Q::State) -> Self {
        Self {
            map: SparseMap::new(),
            state,
        }
    }

    pub(crate) fn init(world: &mut World, config: &mut HandlerConfig) -> Result<Self, InitError> {
        let (ca, state) = Q::init(world, config)?;

        config.push_component_access(ca);

        Ok(FetcherState::new(state))
    }

    #[inline]
    pub(crate) unsafe fn get_unchecked(
        &self,
        entities: &Entities,
        entity: EntityId,
    ) -> Result<Q::Item<'_>, GetError> {
        let Some(loc) = entities.get(entity) else {
            return Err(GetError::NoSuchEntity);
        };

        // Eliminate a branch in `SparseMap::get`.
        assume_debug_checked(loc.archetype != ArchetypeIdx::NULL);

        let Some(state) = self.map.get(loc.archetype) else {
            return Err(GetError::QueryDoesNotMatch);
        };

        Ok(Q::get(state, loc.row))
    }

    #[inline]
    pub(crate) unsafe fn get_many_mut<const N: usize>(
        &mut self,
        entities: &Entities,
        array: [EntityId; N],
    ) -> Result<[Q::Item<'_>; N], GetManyMutError> {
        // Check for overlapping entity ids.
        for i in 0..N {
            for j in 0..i {
                if array[i] == array[j] {
                    return Err(GetManyMutError::AliasedMutability);
                }
            }
        }

        let mut res: [MaybeUninit<Q::Item<'_>>; N] = [(); N].map(|()| MaybeUninit::uninit());

        for i in 0..N {
            match self.get_unchecked(entities, array[i]) {
                Ok(item) => res[i] = MaybeUninit::new(item),
                Err(e) => {
                    if mem::needs_drop::<Q::Item<'_>>() {
                        for item in res.iter_mut().take(i) {
                            item.assume_init_drop();
                        }
                    }

                    return Err(e.into());
                }
            }
        }

        Ok(mem::transmute_copy::<
            [MaybeUninit<Q::Item<'_>>; N],
            [Q::Item<'_>; N],
        >(&res))
    }

    #[inline]
    pub(crate) unsafe fn get_by_location_mut(&mut self, loc: EntityLocation) -> Q::Item<'_> {
        let state = self
            .map
            .get(loc.archetype)
            .expect_debug_checked("invalid entity location");
        Q::get(state, loc.row)
    }

    #[inline]
    pub(crate) unsafe fn iter<'a>(&'a self, archetypes: &'a Archetypes) -> Iter<'a, Q>
    where
        Q: ReadOnlyQuery,
    {
        self.iter_unchecked(archetypes)
    }

    #[inline]
    pub(crate) unsafe fn iter_mut<'a>(&'a mut self, archetypes: &'a Archetypes) -> Iter<'a, Q> {
        self.iter_unchecked(archetypes)
    }

    unsafe fn iter_unchecked<'a>(&'a self, archetypes: &'a Archetypes) -> Iter<'a, Q> {
        let indices = self.map.keys();
        let states = self.map.values();

        assume_debug_checked(indices.len() == states.len());

        if states.is_empty() {
            Iter {
                state: NonNull::dangling(),
                state_last: NonNull::dangling(),
                index: NonNull::dangling(),
                row: ArchetypeRow(0),
                len: 0,
                archetypes,
            }
        } else {
            let start = states.as_ptr().cast_mut();
            let end = start.add(states.len() - 1);
            Iter {
                state: NonNull::new(start).unwrap_debug_checked(),
                state_last: NonNull::new(end).unwrap_debug_checked(),
                index: NonNull::new(indices.as_ptr().cast_mut()).unwrap_debug_checked(),
                row: ArchetypeRow(0),
                len: archetypes
                    .get(indices[0])
                    .unwrap_debug_checked()
                    .entity_count(),
                archetypes,
            }
        }
    }

    #[cfg(feature = "rayon")]
    pub(crate) unsafe fn par_iter<'a>(&'a self, archetypes: &'a Archetypes) -> ParIter<'a, Q>
    where
        Q: ReadOnlyQuery,
    {
        ParIter {
            arch_states: self.map.values(),
            arch_indices: self.map.keys(),
            archetypes,
        }
    }

    #[cfg(feature = "rayon")]
    pub(crate) unsafe fn par_iter_mut<'a>(
        &'a mut self,
        archetypes: &'a Archetypes,
    ) -> ParIter<'a, Q> {
        ParIter {
            arch_states: self.map.values(),
            arch_indices: self.map.keys(),
            archetypes,
        }
    }

    pub(crate) fn refresh_archetype(&mut self, arch: &Archetype) {
        debug_assert!(
            arch.entity_count() != 0,
            "`refresh_archetype` called with empty archetype"
        );

        if let Some(fetch) = Q::new_arch_state(arch, &mut self.state) {
            self.map.insert(arch.index(), fetch);
        }
    }

    pub(crate) fn remove_archetype(&mut self, arch: &Archetype) {
        self.map.remove(arch.index());
    }
}

impl<Q: Query> fmt::Debug for FetcherState<Q> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("FetcherState")
            .field("map", &self.map)
            .field("state", &self.state)
            .finish()
    }
}

/// A [`HandlerParam`] for accessing data from entities matching a given
/// [`Query`].
///
/// For more information, see the relevant [tutorial
/// chapter](crate::tutorial::ch05_fetching).
pub struct Fetcher<'a, Q: Query> {
    state: &'a mut FetcherState<Q>,
    world: UnsafeWorldCell<'a>,
}

impl<'a, Q: Query> Fetcher<'a, Q> {
    /// Returns the read-only query item for the given entity.
    ///
    /// If the entity doesn't exist or doesn't match the query, then a
    /// [`GetError`] is returned.
    #[inline]
    pub fn get(&self, entity: EntityId) -> Result<Q::Item<'_>, GetError>
    where
        Q: ReadOnlyQuery,
    {
        unsafe { self.state.get_unchecked(self.world.entities(), entity) }
    }

    /// Returns the query item for the given entity.
    ///
    /// If the entity doesn't exist or doesn't match the query, then a
    /// [`GetError`] is returned.
    #[inline]
    pub fn get_mut(&mut self, entity: EntityId) -> Result<Q::Item<'_>, GetError> {
        unsafe { self.state.get_unchecked(self.world.entities(), entity) }
    }

    /// Returns the query items for the given array of entities.
    ///
    /// An error of type [`GetManyMutError`] is returned in the following
    /// scenarios:
    /// 1. [`AliasedMutability`] if the given array contains any duplicate
    ///    [`EntityId`]s.
    /// 2. [`NoSuchEntity`] if any of the entities do not exist.
    /// 3. [`QueryDoesNotMatch`] if any of the entities do not match the
    ///    [`Query`] of this fetcher.
    ///
    /// [`AliasedMutability`]: GetManyMutError::AliasedMutability
    /// [`NoSuchEntity`]: GetManyMutError::NoSuchEntity
    /// [`QueryDoesNotMatch`]: GetManyMutError::QueryDoesNotMatch
    #[inline]
    pub fn get_many_mut<const N: usize>(
        &mut self,
        entities: [EntityId; N],
    ) -> Result<[Q::Item<'_>; N], GetManyMutError> {
        unsafe { self.state.get_many_mut(self.world.entities(), entities) }
    }

    /// Returns an iterator over all entities matching the read-only query.
    pub fn iter(&self) -> Iter<Q>
    where
        Q: ReadOnlyQuery,
    {
        unsafe { self.state.iter(self.world.archetypes()) }
    }

    /// Returns an iterator over all entities matching the query.
    pub fn iter_mut(&mut self) -> Iter<Q> {
        unsafe { self.state.iter_mut(self.world.archetypes()) }
    }
}

impl<'a, Q: Query> IntoIterator for Fetcher<'a, Q> {
    type Item = Q::Item<'a>;

    type IntoIter = Iter<'a, Q>;

    fn into_iter(self) -> Self::IntoIter {
        unsafe { self.state.iter_mut(self.world.archetypes()) }
    }
}

impl<'a, Q: ReadOnlyQuery> IntoIterator for &'a Fetcher<'_, Q> {
    type Item = Q::Item<'a>;

    type IntoIter = Iter<'a, Q>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl<'a, Q: Query> IntoIterator for &'a mut Fetcher<'_, Q> {
    type Item = Q::Item<'a>;

    type IntoIter = Iter<'a, Q>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter_mut()
    }
}

impl<'a, Q: Query> fmt::Debug for Fetcher<'a, Q> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Fetcher")
            .field("state", &self.state)
            .field("world", &self.world)
            .finish()
    }
}

/// An error returned when a random-access entity lookup fails.
///
/// See [`Fetcher::get`] and [`Fetcher::get_mut`].
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum GetError {
    /// Entity does not exist.
    NoSuchEntity,
    /// Entity does not match the query.
    QueryDoesNotMatch,
}

impl fmt::Display for GetError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::NoSuchEntity => write!(f, "entity does not exist"),
            Self::QueryDoesNotMatch => write!(f, "entity does not match the query"),
        }
    }
}

#[cfg(feature = "std")]
#[cfg_attr(docsrs, doc(cfg(feature = "std")))]
impl std::error::Error for GetError {}

/// An error returned by [`Fetcher::get_many_mut`].
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum GetManyMutError {
    /// Entity was requested mutably more than once.
    AliasedMutability,
    /// Entity does not exist.
    NoSuchEntity,
    /// Entity does not match the query.
    QueryDoesNotMatch,
}

impl fmt::Display for GetManyMutError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::NoSuchEntity => write!(f, "entity does not exist"),
            Self::QueryDoesNotMatch => write!(f, "entity does not match the query"),
            Self::AliasedMutability => write!(f, "entity was requested mutably more than once"),
        }
    }
}

#[cfg(feature = "std")]
#[cfg_attr(docsrs, doc(cfg(feature = "std")))]
impl std::error::Error for GetManyMutError {}

impl From<GetError> for GetManyMutError {
    fn from(value: GetError) -> Self {
        match value {
            GetError::NoSuchEntity => GetManyMutError::NoSuchEntity,
            GetError::QueryDoesNotMatch => GetManyMutError::QueryDoesNotMatch,
        }
    }
}

unsafe impl<Q> HandlerParam for Fetcher<'_, Q>
where
    Q: Query + 'static,
{
    type State = FetcherState<Q>;

    type Item<'a> = Fetcher<'a, Q>;

    fn init(world: &mut World, config: &mut HandlerConfig) -> Result<Self::State, InitError> {
        FetcherState::init(world, config)
    }

    unsafe fn get<'a>(
        state: &'a mut Self::State,
        _info: &'a HandlerInfo,
        _event_ptr: EventPtr<'a>,
        _target_location: EntityLocation,
        world: UnsafeWorldCell<'a>,
    ) -> Self::Item<'a> {
        Fetcher { state, world }
    }

    fn refresh_archetype(state: &mut Self::State, arch: &Archetype) {
        state.refresh_archetype(arch)
    }

    fn remove_archetype(state: &mut Self::State, arch: &Archetype) {
        state.remove_archetype(arch)
    }
}

/// A [`HandlerParam`] which fetches a single entity from the world.
///
/// If there isn't exactly one entity that matches the [`Query`], a runtime
/// panic occurs. This is useful for representing global variables or singleton
/// entities.
///
/// # Examples
///
/// ```
/// # #[derive(Event)] struct E;
/// use evenio::prelude::*;
///
/// #[derive(Component)]
/// struct MyComponent(i32);
///
/// let mut world = World::new();
///
/// world.add_handler(
///     |_: Receiver<E>, Single(MyComponent(data)): Single<&MyComponent>| {
///         println!("The data is: {data}");
///     },
/// );
///
/// let e = world.spawn();
/// world.insert(e, MyComponent(123));
///
/// world.send(E);
/// ```
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Default, Debug)]
pub struct Single<'a, Q: Query>(pub Q::Item<'a>);

unsafe impl<Q: Query + 'static> HandlerParam for Single<'_, Q> {
    type State = FetcherState<Q>;

    type Item<'a> = Single<'a, Q>;

    fn init(world: &mut World, config: &mut HandlerConfig) -> Result<Self::State, InitError> {
        FetcherState::init(world, config)
    }

    #[track_caller]
    unsafe fn get<'a>(
        state: &'a mut Self::State,
        info: &'a HandlerInfo,
        event_ptr: EventPtr<'a>,
        target_location: EntityLocation,
        world: UnsafeWorldCell<'a>,
    ) -> Self::Item<'a> {
        match TrySingle::get(state, info, event_ptr, target_location, world) {
            TrySingle(Ok(item)) => Single(item),
            TrySingle(Err(e)) => {
                panic!(
                    "failed to fetch exactly one entity matching the query `{}`: {e}",
                    any::type_name::<Q>()
                )
            }
        }
    }

    fn refresh_archetype(state: &mut Self::State, arch: &Archetype) {
        state.refresh_archetype(arch)
    }

    fn remove_archetype(state: &mut Self::State, arch: &Archetype) {
        state.remove_archetype(arch)
    }
}

/// Like [`Single`], but contains a `Result` instead of panicking on error.
///
/// This is useful if you need to explicitly handle the situation where the
/// query does not match exactly one entity.
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct TrySingle<'a, Q: Query>(pub Result<Q::Item<'a>, SingleError>);

unsafe impl<Q: Query + 'static> HandlerParam for TrySingle<'_, Q> {
    type State = FetcherState<Q>;

    type Item<'a> = TrySingle<'a, Q>;

    fn init(world: &mut World, config: &mut HandlerConfig) -> Result<Self::State, InitError> {
        FetcherState::init(world, config)
    }

    unsafe fn get<'a>(
        state: &'a mut Self::State,
        _info: &'a HandlerInfo,
        _event_ptr: EventPtr<'a>,
        _target_location: EntityLocation,
        world: UnsafeWorldCell<'a>,
    ) -> Self::Item<'a> {
        let mut it = state.iter_mut(world.archetypes());

        let Some(item) = it.next() else {
            return TrySingle(Err(SingleError::QueryDoesNotMatch));
        };

        if it.next().is_some() {
            return TrySingle(Err(SingleError::MoreThanOneMatch));
        }

        TrySingle(Ok(item))
    }

    fn refresh_archetype(state: &mut Self::State, arch: &Archetype) {
        state.refresh_archetype(arch)
    }

    fn remove_archetype(state: &mut Self::State, arch: &Archetype) {
        state.remove_archetype(arch)
    }
}

/// Error raised when fetching exactly one entity matching a query fails.
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum SingleError {
    /// Query does not match any entities
    QueryDoesNotMatch,
    /// More than one entity matched the query.
    MoreThanOneMatch,
}

impl fmt::Display for SingleError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let msg = match self {
            SingleError::QueryDoesNotMatch => "query does not match any entities",
            SingleError::MoreThanOneMatch => "more than one entity matched the query",
        };

        write!(f, "{msg}")
    }
}

#[cfg(feature = "std")]
#[cfg_attr(docsrs, doc(cfg(feature = "std")))]
impl std::error::Error for SingleError {}

/// Iterator over entities matching the query `Q`.
///
/// Entities are visited in a deterministic but otherwise unspecified order.
#[must_use = "iterators are lazy and do nothing unless consumed"]
pub struct Iter<'a, Q: Query> {
    /// Pointer into the array of archetype states. This pointer moves forward
    /// until it reaches `state_last`.
    state: NonNull<Q::ArchState>,
    /// Pointer to the last arch state, or dangling if there are no arch states.
    /// This is _not_ a one-past-the-end pointer.
    state_last: NonNull<Q::ArchState>,
    /// Pointer into the array of archetype indices. This pointer moves forward
    /// in lockstep with `state`.
    index: NonNull<ArchetypeIdx>,
    /// Current row of the current archetype.
    row: ArchetypeRow,
    /// Number of entities in the current archetype.
    len: u32,
    archetypes: &'a Archetypes,
}

impl<'a, Q: Query> Iterator for Iter<'a, Q> {
    type Item = Q::Item<'a>;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        if self.row.0 == self.len {
            if self.state == self.state_last {
                return None;
            }

            self.state = unsafe { NonNull::new_unchecked(self.state.as_ptr().add(1)) };
            self.index = unsafe { NonNull::new_unchecked(self.index.as_ptr().add(1)) };

            let idx = unsafe { *self.index.as_ptr() };
            let arch = unsafe { self.archetypes.get(idx).unwrap_debug_checked() };

            self.row = ArchetypeRow(0);
            self.len = arch.entity_count();

            // SAFETY: Fetcher state only contains nonempty archetypes.
            unsafe { assume_debug_checked(self.len > 0) };
        }

        let state = unsafe { &*self.state.as_ptr().cast_const() };
        let item = unsafe { Q::get(state, self.row) };

        self.row.0 += 1;

        Some(item)
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let len = self.len();
        (len, Some(len))
    }
}

impl<Q: Query> ExactSizeIterator for Iter<'_, Q> {
    fn len(&self) -> usize {
        let mut remaining = self.len - self.row.0;

        let mut index = self.index.as_ptr();

        let index_last = unsafe {
            // TODO: use `.sub_ptr` when stabilized.
            index.add(self.state_last.as_ptr().offset_from(self.state.as_ptr()) as usize)
        };

        while index != index_last {
            index = unsafe { index.add(1) };

            remaining +=
                unsafe { self.archetypes.get(*index).unwrap_debug_checked() }.entity_count();
        }

        remaining as usize
    }
}

impl<Q: Query> FusedIterator for Iter<'_, Q> {}

// SAFETY: Iter is only cloneable when the query is read-only.
impl<'a, Q: ReadOnlyQuery> Clone for Iter<'a, Q> {
    fn clone(&self) -> Self {
        Self {
            state: self.state,
            state_last: self.state_last,
            index: self.index,
            row: self.row,
            len: self.len,
            archetypes: self.archetypes,
        }
    }
}

impl<Q: Query> fmt::Debug for Iter<'_, Q> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Iter")
            .field("state", &self.state)
            .field("state_last", &self.state_last)
            .field("index", &self.index)
            .field("row", &self.row)
            .field("len", &self.len)
            .field("archetypes", &self.archetypes)
            .finish()
    }
}

// SAFETY: `Iter` iterates over component data only, which is always `Send` and
// `Sync`.
unsafe impl<Q: Query> Send for Iter<'_, Q> {}
unsafe impl<Q: Query> Sync for Iter<'_, Q> {}

#[cfg(feature = "rayon")]
#[cfg_attr(docsrs, doc(cfg(feature = "rayon")))]
pub use rayon_impl::*;

#[cfg(feature = "rayon")]
mod rayon_impl {
    use rayon::iter::plumbing::UnindexedConsumer;
    use rayon::prelude::*;

    use super::*;

    /// A [`ParallelIterator`] over entities matching the query `Q`.
    ///
    /// This is the parallel version of [`Iter`].
    #[must_use = "iterators are lazy and do nothing unless consumed"]
    pub struct ParIter<'a, Q: Query> {
        pub(super) arch_states: &'a [Q::ArchState],
        pub(super) arch_indices: &'a [ArchetypeIdx],
        pub(super) archetypes: &'a Archetypes,
    }

    impl<Q: ReadOnlyQuery> Clone for ParIter<'_, Q> {
        fn clone(&self) -> Self {
            Self {
                arch_states: self.arch_states,
                arch_indices: self.arch_indices,
                archetypes: self.archetypes,
            }
        }
    }

    impl<Q: Query> fmt::Debug for ParIter<'_, Q> {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            f.debug_struct("ParIter")
                .field("arch_states", &self.arch_states)
                .field("arch_indices", &self.arch_indices)
                .field("archetypes", &self.archetypes)
                .finish()
        }
    }

    impl<'a, Q> ParallelIterator for ParIter<'a, Q>
    where
        Q: Query,
        Q::Item<'a>: Send,
    {
        type Item = Q::Item<'a>;

        fn drive_unindexed<C>(self, consumer: C) -> C::Result
        where
            C: UnindexedConsumer<Self::Item>,
        {
            unsafe { assume_debug_checked(self.arch_states.len() == self.arch_indices.len()) };

            self.arch_states
                .par_iter()
                .zip_eq(self.arch_indices)
                .flat_map(|(state, &index)| {
                    let entity_count =
                        unsafe { self.archetypes.get(index).unwrap_debug_checked() }.entity_count();

                    (0..entity_count).into_par_iter().map(|row| {
                        let item: Q::Item<'a> = unsafe { Q::get(state, ArchetypeRow(row)) };
                        item
                    })
                })
                .drive_unindexed(consumer)
        }
    }

    #[cfg_attr(docsrs, doc(cfg(feature = "rayon")))]
    impl<'a, Q> IntoParallelIterator for Fetcher<'a, Q>
    where
        Q: Query,
        Q::Item<'a>: Send,
    {
        type Iter = ParIter<'a, Q>;

        type Item = Q::Item<'a>;

        fn into_par_iter(self) -> Self::Iter {
            unsafe { self.state.par_iter_mut(self.world.archetypes()) }
        }
    }

    #[cfg_attr(docsrs, doc(cfg(feature = "rayon")))]
    impl<'a, Q> IntoParallelIterator for &'a Fetcher<'_, Q>
    where
        Q: ReadOnlyQuery,
        Q::Item<'a>: Send,
    {
        type Iter = ParIter<'a, Q>;

        type Item = Q::Item<'a>;

        fn into_par_iter(self) -> Self::Iter {
            unsafe { self.state.par_iter(self.world.archetypes()) }
        }
    }

    #[cfg_attr(docsrs, doc(cfg(feature = "rayon")))]
    impl<'a, Q: Query> IntoParallelIterator for &'a mut Fetcher<'_, Q>
    where
        Q: Query,
        Q::Item<'a>: Send,
    {
        type Iter = ParIter<'a, Q>;

        type Item = Q::Item<'a>;

        fn into_par_iter(self) -> Self::Iter {
            unsafe { self.state.par_iter_mut(self.world.archetypes()) }
        }
    }
}

#[cfg(test)]
mod tests {
    use alloc::collections::BTreeSet;
    use core::sync::atomic::{AtomicUsize, Ordering};

    use super::*;
    use crate::prelude::*;

    #[derive(Event)]
    struct E1;

    #[derive(Event)]
    struct E2;

    #[derive(Event)]
    struct E3;

    #[derive(Component, PartialEq, Eq, Debug)]
    struct C1(u32);

    #[derive(Component, PartialEq, Eq, Debug)]
    struct C2(u32);

    #[derive(Component, PartialEq, Eq, Debug)]
    struct C3(u32);

    #[test]
    fn random_access() {
        let mut world = World::new();

        let e = world.spawn();
        let e2 = world.spawn();
        let e3 = world.spawn();
        world.spawn();

        world.insert(e, C1(123));
        world.insert(e2, C1(456));
        world.insert(e3, C2(789));

        world.add_handler(move |_: Receiver<E1>, f: Fetcher<&C1>| {
            assert_eq!(f.get(e), Ok(&C1(123)));
        });

        world.add_handler(move |_: Receiver<E2>, f: Fetcher<&C2>| {
            assert_eq!(f.get(e3), Ok(&C2(789)))
        });

        world.send(E1);

        world.add_handler(|_: Receiver<E2>, f: Fetcher<&C1>| {
            assert_eq!(f.get(EntityId::NULL), Err(GetError::NoSuchEntity));
        });

        world.send(E2);

        world.add_handler(move |_: Receiver<E3>, f: Fetcher<&C2>| {
            assert_eq!(f.get(e), Err(GetError::QueryDoesNotMatch))
        });

        world.send(E3);
    }

    #[test]
    fn fetch_many_mut_drop() {
        static COUNT: AtomicUsize = AtomicUsize::new(0);

        #[derive(Query, PartialEq, Debug)]
        struct CustomQuery;

        impl Drop for CustomQuery {
            fn drop(&mut self) {
                COUNT.fetch_add(1, Ordering::SeqCst);
            }
        }

        let mut world = World::new();

        let e1 = world.spawn();
        let e2 = world.spawn();
        let e3 = world.spawn();

        world.add_handler(move |_: Receiver<E1>, mut f: Fetcher<CustomQuery>| {
            assert_eq!(
                f.get_many_mut([e1, e2, EntityId::NULL, e3]),
                Err(GetManyMutError::NoSuchEntity)
            );
        });

        world.send(E1);

        assert_eq!(COUNT.load(Ordering::SeqCst), 2);
    }

    #[test]
    fn iter() {
        let mut world = World::new();

        let mut set = BTreeSet::new();

        for i in 0..20_u32 {
            let e = world.spawn();

            world.insert(e, C1(i.pow(2)));

            if i % 2 == 0 {
                world.insert(e, C2(i.pow(2)));
            }

            if i % 3 == 0 {
                world.insert(e, C3(i.pow(2)));
            }

            set.insert(i.pow(2));
        }

        world.add_handler(move |_: Receiver<E1>, f: Fetcher<&C1>| {
            for c in f {
                assert!(set.remove(&c.0));
            }

            assert!(set.is_empty());
        });

        world.send(E1);
    }

    #[cfg(feature = "rayon")]
    #[test]
    fn par_iter() {
        use rayon::prelude::*;

        let mut world = World::new();

        const N: u32 = 20;

        for i in 0..N {
            let e = world.spawn();

            world.insert(e, C1(i));

            if i % 2 == 0 {
                world.insert(e, C2(i));
            }

            if i % 3 == 0 {
                world.insert(e, C3(i));
            }
        }

        world.add_handler(move |_: Receiver<E1>, f: Fetcher<&C1>| {
            let sum = f.par_iter().map(|c| c.0).sum::<u32>();
            assert_eq!(sum, N * (N - 1) / 2);
        });

        world.send(E1);
    }

    #[test]
    fn iter_empty() {
        let mut world = World::new();

        world.add_handler(move |_: Receiver<E1>, f: Fetcher<&C1>| {
            for c in f {
                println!("{c:?}");
            }
        });

        world.send(E1);
    }

    #[test]
    fn iter_previously_nonempty() {
        let mut world = World::new();

        world.add_handler(move |_: Receiver<E1>, f: Fetcher<EntityId>| {
            for id in f {
                println!("{id:?}");
            }
        });

        let e = world.spawn();
        world.send(E1);
        world.despawn(e);
        world.send(E1);
    }

    #[test]
    fn iter_len() {
        let mut world = World::new();

        let count = 20;

        for i in 1..=count {
            let e = world.spawn();
            world.insert(e, C1(i));

            if i % 2 == 0 {
                world.insert(e, C2(i));
            }

            if i % 3 == 0 {
                world.insert(e, C3(i));
            }
        }

        world.add_handler(
            move |_: Receiver<E1>, f1: Fetcher<&C1>, f2: Fetcher<&C2>, f3: Fetcher<&C3>| {
                assert_eq!(f1.iter().len(), count as usize);
                assert_eq!(f2.iter().len(), count as usize / 2);
                assert_eq!(f3.iter().len(), count as usize / 3);
            },
        );

        world.send(E1);
    }

    #[test]
    fn single_param() {
        let mut world = World::new();

        {
            let e = world.spawn();
            world.insert(e, C1(123));
        }

        world.add_handler(|_: Receiver<E1>, Single(&C1(n)): Single<&C1>| {
            assert_eq!(n, 123);
        });

        world.send(E1);
    }

    #[test]
    #[should_panic]
    fn single_param_panics_on_zero() {
        let mut world = World::new();

        world.add_handler(|_: Receiver<E1>, _: Single<&C1>| {});

        world.send(E1);
    }

    #[test]
    #[should_panic]
    fn single_param_panics_on_many() {
        let mut world = World::new();

        {
            let e = world.spawn();
            world.insert(e, C1(123));
            let e = world.spawn();
            world.insert(e, C1(456));
        }

        world.add_handler(|_: Receiver<E1>, _: Single<&C1>| {});

        world.send(E1);
    }

    #[test]
    fn try_single_param() {
        let mut world = World::new();

        {
            let e = world.spawn();
            world.insert(e, C2(123));

            let e = world.spawn();
            world.insert(e, C3(123));
            let e = world.spawn();
            world.insert(e, C3(456));
        }

        world.add_handler(
            |_: Receiver<E1>, s1: TrySingle<&C1>, s2: TrySingle<&C2>, s3: TrySingle<&C3>| {
                assert_eq!(s1.0, Err(SingleError::QueryDoesNotMatch));
                assert_eq!(s2.0, Ok(&C2(123)));
                assert_eq!(s3.0, Err(SingleError::MoreThanOneMatch));
            },
        );

        world.send(E1);
    }

    fn _assert_auto_trait_impls()
    where
        for<'a> Fetcher<'a, ()>: Send + Sync,
        for<'a, 'b> Fetcher<'a, &'b C1>: Send + Sync,
        for<'a, 'b, 'c> Fetcher<'a, (&'b C1, &'c mut C2)>: Send + Sync,
    {
    }
}
