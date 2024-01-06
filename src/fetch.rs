use core::marker::PhantomData;
use core::{any, fmt};
use std::iter::FusedIterator;
use std::ptr::NonNull;

use crate::archetype::{Archetype, ArchetypeIdx, ArchetypeRow, Archetypes};
use crate::debug_checked::{assume_debug_checked, UnwrapDebugChecked};
use crate::entity::{Entities, EntityId};
use crate::event::EventPtr;
use crate::query::{Query, ReadOnlyQuery};
use crate::sparse_map::SparseMap;
use crate::system::{Config, InitError, SystemInfo, SystemParam};
use crate::world::{UnsafeWorldCell, World};

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

    pub(crate) fn init(world: &mut World, config: &mut Config) -> Result<Self, InitError> {
        let (expr, state) = Q::init(world, config)?;

        let res = FetcherState::new(state);

        match expr.or(&config.component_access) {
            Ok(new_component_access) => config.component_access = new_component_access,
            Err(_) => {
                return Err(InitError(
                    format!(
                        "query `{}` has incompatible component access with previous queries in \
                         this system",
                        any::type_name::<Q>()
                    )
                    .into(),
                ))
            }
        }

        Ok(res)
    }

    #[inline]
    pub(crate) unsafe fn get(
        &self,
        entities: &Entities,
        entity: EntityId,
    ) -> Result<Q::Item<'_>, GetError>
    where
        Q: ReadOnlyQuery,
    {
        let Some(loc) = entities.get(entity) else {
            return Err(GetError::NoSuchEntity(entity));
        };

        // Eliminate a panicking branch.
        assume_debug_checked(loc.archetype != ArchetypeIdx::NULL);

        let Some(state) = self.map.get(loc.archetype) else {
            return Err(GetError::QueryDoesNotMatch(entity));
        };

        Ok(Q::fetch(state, loc.row))
    }

    #[inline]
    pub(crate) unsafe fn get_mut(
        &mut self,
        entities: &Entities,
        entity: EntityId,
    ) -> Result<Q::Item<'_>, GetError> {
        let Some(loc) = entities.get(entity) else {
            return Err(GetError::NoSuchEntity(entity));
        };

        // Eliminate a panicking branch.
        assume_debug_checked(loc.archetype != ArchetypeIdx::NULL);

        // TODO: Resize the sparse array so that all valid archetype indices are in
        // bounds, and then `assume` it. That would eliminate a bounds check.

        let Some(state) = self.map.get_mut(loc.archetype) else {
            return Err(GetError::QueryDoesNotMatch(entity));
        };

        Ok(Q::fetch(state, loc.row))
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

        let first_arch_len = indices.first().map_or(0, |&idx| {
            unsafe { archetypes.get(idx).unwrap_debug_checked() }.entity_count()
        });

        let state = NonNull::new(states.as_ptr().cast_mut()).unwrap_or(NonNull::dangling());
        let state_last = if states.is_empty() {
            NonNull::dangling()
        } else {
            NonNull::new(state.as_ptr().add(states.len() - 1)).unwrap_debug_checked()
        };

        let index = NonNull::new(indices.as_ptr().cast_mut()).unwrap_or(NonNull::dangling());

        Iter {
            state,
            state_last,
            index,
            row: ArchetypeRow(0),
            len: first_arch_len,
            archetypes,
            _marker: PhantomData,
        }
    }

    pub(crate) unsafe fn refresh_archetype(&mut self, arch: &Archetype) {
        if let Some(fetch) = Q::new_arch_state(arch, &mut self.state) {
            self.map.insert(arch.index(), fetch);
        }
    }

    pub(crate) unsafe fn remove_archetype(&mut self, arch: &Archetype) {
        self.map.remove(arch.index());
    }

    // TODO: get_many_mut
}

impl<Q: Query> fmt::Debug for FetcherState<Q> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("FetcherState")
            .field("map", &self.map)
            .field("state", &self.state)
            .finish()
    }
}

pub struct Fetcher<'a, Q: Query> {
    state: &'a mut FetcherState<Q>,
    world: UnsafeWorldCell<'a>,
}

impl<'a, Q: Query> Fetcher<'a, Q> {
    pub(crate) unsafe fn new(state: &'a mut FetcherState<Q>, world: UnsafeWorldCell<'a>) -> Self {
        Self { state, world }
    }

    #[inline]
    pub fn get(&self, entity: EntityId) -> Result<Q::Item<'_>, GetError>
    where
        Q: ReadOnlyQuery,
    {
        unsafe { self.state.get(self.world.entities(), entity) }
    }

    #[inline]
    pub fn get_mut(&mut self, entity: EntityId) -> Result<Q::Item<'_>, GetError> {
        unsafe { self.state.get_mut(self.world.entities(), entity) }
    }

    pub fn iter(&self) -> Iter<Q>
    where
        Q: ReadOnlyQuery,
    {
        unsafe { self.state.iter(self.world.archetypes()) }
    }

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

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum GetError {
    NoSuchEntity(EntityId),
    QueryDoesNotMatch(EntityId),
    AliasedMutability(EntityId),
}

impl fmt::Display for GetError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            GetError::NoSuchEntity(id) => write!(f, "entity {id:?} does not exist"),
            GetError::QueryDoesNotMatch(id) => {
                write!(f, "entity {id:?} does not match the query")
            }
            GetError::AliasedMutability(id) => {
                write!(f, "entity {id:?} was requested mutably more than once")
            }
        }
    }
}

#[cfg(feature = "std")]
impl std::error::Error for GetError {}

impl<Q> SystemParam for Fetcher<'_, Q>
where
    Q: Query + 'static,
{
    type State = FetcherState<Q>;

    type Item<'a> = Fetcher<'a, Q>;

    fn init(world: &mut World, config: &mut Config) -> Result<Self::State, InitError> {
        FetcherState::init(world, config)
    }

    unsafe fn get_param<'a>(
        state: &'a mut Self::State,
        _info: &'a SystemInfo,
        _event_ptr: EventPtr<'a>,
        world: UnsafeWorldCell<'a>,
    ) -> Self::Item<'a> {
        Fetcher { state, world }
    }

    unsafe fn refresh_archetype(state: &mut Self::State, arch: &Archetype) {
        state.refresh_archetype(arch)
    }

    unsafe fn remove_archetype(state: &mut Self::State, arch: &Archetype) {
        state.remove_archetype(arch)
    }
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Default, Debug)]
pub struct Single<'a, Q: Query>(pub Q::Item<'a>);

impl<Q: Query + 'static> SystemParam for Single<'_, Q> {
    type State = FetcherState<Q>;

    type Item<'a> = Single<'a, Q>;

    fn init(world: &mut World, config: &mut Config) -> Result<Self::State, InitError> {
        FetcherState::init(world, config)
    }

    #[track_caller]
    unsafe fn get_param<'a>(
        state: &'a mut Self::State,
        info: &'a SystemInfo,
        event_ptr: EventPtr<'a>,
        world: UnsafeWorldCell<'a>,
    ) -> Self::Item<'a> {
        match TrySingle::get_param(state, info, event_ptr, world) {
            TrySingle(Ok(item)) => Single(item),
            TrySingle(Err(e)) => {
                panic!(
                    "failed to fetch exactly one entity matching the query `{}`: {e}",
                    any::type_name::<Q>()
                )
            }
        }
    }

    unsafe fn refresh_archetype(state: &mut Self::State, arch: &Archetype) {
        state.refresh_archetype(arch)
    }

    unsafe fn remove_archetype(state: &mut Self::State, arch: &Archetype) {
        state.remove_archetype(arch)
    }
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct TrySingle<'a, Q: Query>(pub Result<Q::Item<'a>, SingleError>);

impl<Q: Query + 'static> SystemParam for TrySingle<'_, Q> {
    type State = FetcherState<Q>;

    type Item<'a> = TrySingle<'a, Q>;

    fn init(world: &mut World, config: &mut Config) -> Result<Self::State, InitError> {
        FetcherState::init(world, config)
    }

    unsafe fn get_param<'a>(
        state: &'a mut Self::State,
        _info: &'a SystemInfo,
        _event_ptr: EventPtr<'a>,
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

    unsafe fn refresh_archetype(state: &mut Self::State, arch: &Archetype) {
        state.refresh_archetype(arch)
    }

    unsafe fn remove_archetype(state: &mut Self::State, arch: &Archetype) {
        state.refresh_archetype(arch)
    }
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum SingleError {
    QueryDoesNotMatch,
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
impl std::error::Error for SingleError {}

/// Iterator over entities matching the cached query `Q`.
///
/// Entities are visited in a deterministic but otherwise unspecified order.
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
    // Iterator should inherit the variance of the query item.
    _marker: PhantomData<Q::Item<'a>>,
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
        let item = unsafe { Q::fetch(state, self.row) };

        self.row.0 += 1;

        Some(item)
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let len = self.len();
        (len, Some(len))
    }

    // TODO: override `.nth()`?
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
            _marker: self._marker,
        }
    }
}

impl<'a, Q: Query> fmt::Debug for Iter<'a, Q> {
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

// TODO `Send` and `Sync` impls for `Iter`.

#[cfg(test)]
mod tests {
    use std::collections::BTreeSet;

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
    fn random_access_cached() {
        let mut world = World::new();

        let e = world.spawn();
        let e2 = world.spawn();
        let e3 = world.spawn();
        world.spawn();

        world.insert(e, C1(123));
        world.insert(e2, C1(456));
        world.insert(e3, C2(789));

        world.add_system(move |_: Receiver<E1>, f: Fetcher<&C1>| {
            assert_eq!(f.get(e), Ok(&C1(123)));
        });

        world.add_system(move |_: Receiver<E2>, f: Fetcher<&C2>| {
            assert_eq!(f.get(e3), Ok(&C2(789)))
        });

        world.send(E1);

        world.add_system(|_: Receiver<E2>, f: Fetcher<&C1>| {
            assert_eq!(
                f.get(EntityId::NULL),
                Err(GetError::NoSuchEntity(EntityId::NULL))
            );
        });

        world.send(E2);

        world.add_system(move |_: Receiver<E3>, f: Fetcher<&C2>| {
            assert_eq!(f.get(e), Err(GetError::QueryDoesNotMatch(e)))
        });

        world.send(E3);
    }

    #[test]
    fn iter_cached() {
        let mut world = World::new();

        let mut set = BTreeSet::new();

        for i in 0..100_u32 {
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

        world.add_system(move |_: Receiver<E1>, f: Fetcher<&C1>| {
            for c in f {
                assert!(set.remove(&c.0));
            }

            assert!(set.is_empty());
        });

        world.send(E1);
    }

    #[test]
    fn iter_cached_empty() {
        let mut world = World::new();

        world.add_system(move |_: Receiver<E1>, f: Fetcher<&C1>| {
            for _ in f {
                std::hint::black_box(());
            }
        });

        world.send(E1);
    }

    #[test]
    fn iter_cached_len() {
        let mut world = World::new();

        let count = 100;

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

        world.add_system(
            move |_: Receiver<E1>, f1: Fetcher<&C1>, f2: Fetcher<&C2>, f3: Fetcher<&C3>| {
                dbg!(&f1);

                assert_eq!(f1.iter().len(), count as usize);
                assert_eq!(f2.iter().len(), count as usize / 2);
                assert_eq!(f3.iter().len(), count as usize / 3);
            },
        );

        // dbg!(world.archetypes());

        world.send(E1);
    }

    #[test]
    fn single_param() {
        let mut world = World::new();

        {
            let e = world.spawn();
            world.insert(e, C1(123));
        }

        world.add_system(|_: Receiver<E1>, Single(&C1(n)): Single<&C1>| {
            assert_eq!(n, 123);
        });

        world.send(E1);
    }

    #[test]
    #[should_panic]
    fn single_param_panics_on_zero() {
        let mut world = World::new();

        world.add_system(|_: Receiver<E1>, _: Single<&C1>| {});

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

        world.add_system(|_: Receiver<E1>, _: Single<&C1>| {});

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

        world.add_system(
            |_: Receiver<E1>, s1: TrySingle<&C1>, s2: TrySingle<&C2>, s3: TrySingle<&C3>| {
                assert_eq!(s1.0, Err(SingleError::QueryDoesNotMatch));
                assert_eq!(s2.0, Ok(&C2(123)));
                assert_eq!(s3.0, Err(SingleError::MoreThanOneMatch));
            },
        );

        world.send(E1);
    }
}
