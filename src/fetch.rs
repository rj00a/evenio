use core::marker::PhantomData;
use core::{any, fmt};
use std::iter::FusedIterator;
use std::ptr::NonNull;

use crate::archetype::{Archetype, ArchetypeIdx, ArchetypeRow, Archetypes};
use crate::debug_checked::{assume_debug_checked, UnwrapDebugChecked};
use crate::entity::{Entities, EntityId};
use crate::query::{Query, ReadOnlyQuery};
use crate::sparse_map::SparseMap;
use crate::system::{Config, InitError, SystemParam};
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

        match expr.and(&config.component_access) {
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
    ) -> Result<Q::Item<'_>, FetchError>
    where
        Q: ReadOnlyQuery,
    {
        let Some(loc) = entities.get(entity) else {
            return Err(FetchError::NoSuchEntity(entity));
        };

        // Eliminate a panicking branch.
        assume_debug_checked(loc.archetype != ArchetypeIdx::NULL);

        let Some(state) = self.map.get(loc.archetype) else {
            return Err(FetchError::QueryDoesNotMatch(entity));
        };

        Ok(Q::fetch(state, loc.row))
    }

    #[inline]
    pub(crate) unsafe fn get_mut(
        &mut self,
        entities: &Entities,
        entity: EntityId,
    ) -> Result<Q::Item<'_>, FetchError> {
        let Some(loc) = entities.get(entity) else {
            return Err(FetchError::NoSuchEntity(entity));
        };

        // Eliminate a panicking branch.
        assume_debug_checked(loc.archetype != ArchetypeIdx::NULL);

        // TODO: Resize the sparse array so that all valid archetype indices are in
        // bounds, and then `assume` it. That would eliminate a bounds check.

        let Some(state) = self.map.get_mut(loc.archetype) else {
            return Err(FetchError::QueryDoesNotMatch(entity));
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

        Iter {
            state: states.first().map_or(NonNull::dangling(), NonNull::from),
            state_last: states.last().map_or(NonNull::dangling(), NonNull::from),
            index: indices.first().map_or(NonNull::dangling(), NonNull::from),
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
    pub fn get(&self, entity: EntityId) -> Result<Q::Item<'_>, FetchError>
    where
        Q: ReadOnlyQuery,
    {
        unsafe { self.state.get(self.world.entities(), entity) }
    }

    #[inline]
    pub fn get_mut(&mut self, entity: EntityId) -> Result<Q::Item<'_>, FetchError> {
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

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum FetchError {
    NoSuchEntity(EntityId),
    QueryDoesNotMatch(EntityId),
    AliasedMutability(EntityId),
}

impl fmt::Display for FetchError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FetchError::NoSuchEntity(id) => write!(f, "the entity {id:?} does not exist"),
            FetchError::QueryDoesNotMatch(id) => {
                write!(f, "the components of entity {id:?} do not match the query")
            }
            FetchError::AliasedMutability(id) => {
                write!(f, "the entity {id:?} was requested mutably more than once")
            }
        }
    }
}

#[cfg(feature = "std")]
impl std::error::Error for FetchError {}

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
        system_info: &'a crate::system::SystemInfo,
        event_ptr: crate::event::EventPtr<'a>,
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
pub struct FetchOne<'a, Q: Query>(pub Q::Item<'a>);

impl<Q: Query + 'static> SystemParam for FetchOne<'_, Q> {
    type State = FetcherState<Q>;

    type Item<'a> = FetchOne<'a, Q>;

    fn init(world: &mut World, config: &mut Config) -> Result<Self::State, InitError> {
        FetcherState::init(world, config)
    }

    unsafe fn get_param<'a>(
        state: &'a mut Self::State,
        system_info: &'a crate::system::SystemInfo,
        event_ptr: crate::event::EventPtr<'a>,
        world: UnsafeWorldCell<'a>,
    ) -> Self::Item<'a> {
        todo!()
    }
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Default, Debug)]
pub struct TryFetchOne<'a, Q: Query>(pub Option<Q::Item<'a>>);

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

// SAFETY: Iter is only cloneable when the query item is read-only.
impl<'a, Q: ReadOnlyQuery> Clone for Iter<'a, Q> {
    fn clone(&self) -> Self {
        Self {
            state: self.state.clone(),
            state_last: self.state_last.clone(),
            index: self.index.clone(),
            row: self.row.clone(),
            len: self.len.clone(),
            archetypes: self.archetypes.clone(),
            _marker: self._marker.clone(),
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
    struct C1(i32);

    #[derive(Component, PartialEq, Eq, Debug)]
    struct C2(i32);

    #[derive(Component, PartialEq, Eq, Debug)]
    struct C3(i32);

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
                Err(FetchError::NoSuchEntity(EntityId::NULL))
            );
        });

        world.send(E2);

        world.add_system(move |_: Receiver<E3>, f: Fetcher<&C2>| {
            assert_eq!(f.get(e), Err(FetchError::QueryDoesNotMatch(e)))
        });

        world.send(E3);
    }

    #[test]
    fn iter_cached() {
        let mut world = World::new();

        let mut set = BTreeSet::new();

        for i in 0..100_i32 {
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
                assert_eq!(set.take(&c.0), None);
            }

            assert!(set.is_empty());
        });
    }
}
