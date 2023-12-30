use core::fmt;
use std::marker::PhantomData;
use std::{any, mem};

use crate::access::ComponentAccessExpr;
use crate::archetype::{Archetype, ArchetypeIdx, ArchetypeRow, Archetypes};
use crate::debug_checked::assume_debug_checked;
use crate::entity::EntityId;
use crate::query::{Query, ReadOnlyQuery};
use crate::sparse_map::SparseMap;
use crate::system::{Config, InitError, RefreshArchetypeReason, SystemParam};
use crate::world::{UnsafeWorldCell, World};

pub struct FetcherState<Q: Query> {
    map: SparseMap<ArchetypeIdx, Q::Fetch>,
    state: Q::State,
}

impl<Q: Query> FetcherState<Q> {
    pub(crate) fn new(
        world: UnsafeWorldCell,
        expr: &ComponentAccessExpr,
        mut state: Q::State,
    ) -> Self {
        let mut map = SparseMap::new();

        // TODO: use an index to make this faster?
        for arch in world.archetypes().iter() {
            if let Some(columns) = Q::init_fetch(arch, &mut state) {
                map.insert(arch.index(), columns);
            }
        }

        Self { map, state }
    }

    #[inline]
    pub(crate) unsafe fn get(
        &self,
        world: UnsafeWorldCell,
        entity: EntityId,
    ) -> Result<Q::Item<'_>, FetchError>
    where
        Q: ReadOnlyQuery,
    {
        let entities = world.entities();

        let Some(loc) = entities.get(entity) else {
            return Err(FetchError::NoSuchEntity(entity));
        };

        // Eliminate a panicking branch.
        assume_debug_checked(loc.archetype != ArchetypeIdx::NULL);

        let Some(fetch) = self.map.get(loc.archetype) else {
            return Err(FetchError::QueryDoesNotMatch(entity));
        };

        Ok(Q::fetch(fetch, loc.row))
    }

    #[inline]
    pub(crate) unsafe fn get_mut(
        &mut self,
        world: UnsafeWorldCell,
        entity: EntityId,
    ) -> Result<Q::Item<'_>, FetchError> {
        let entities = world.entities();

        let Some(loc) = entities.get(entity) else {
            return Err(FetchError::NoSuchEntity(entity));
        };

        // Eliminate a panicking branch.
        assume_debug_checked(loc.archetype != ArchetypeIdx::NULL);

        // TODO: Resize the sparse array so that all valid archetype indices are in
        // bounds, and then `assume` it. That would eliminate a bounds check.

        let Some(fetch) = self.map.get_mut(loc.archetype) else {
            return Err(FetchError::QueryDoesNotMatch(entity));
        };

        Ok(Q::fetch_mut(fetch, loc.row))
    }

    pub(crate) unsafe fn refresh_archetype(
        &mut self,
        reason: RefreshArchetypeReason,
        arch: &Archetype,
    ) {
        match reason {
            RefreshArchetypeReason::New
            | RefreshArchetypeReason::RefreshPointers
            | RefreshArchetypeReason::Nonempty => {
                if let Some(fetch) = Q::init_fetch(arch, &mut self.state) {
                    self.map.insert(arch.index(), fetch);
                }
            }
            RefreshArchetypeReason::Empty => {
                self.map.remove(arch.index());
            }
        }
    }

    // TODO: get_many_mut
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
        unsafe { self.state.get(self.world, entity) }
    }

    #[inline]
    pub fn get_mut(&mut self, entity: EntityId) -> Result<Q::Item<'_>, FetchError> {
        unsafe { self.state.get_mut(self.world, entity) }
    }

    pub fn iter(&self) -> Iter<Q>
    where
        Q: ReadOnlyQuery,
    {
        Iter {
            fetches: &self.state.map.values(),
            row: ArchetypeRow(0),
            archetypes: self.world.archetypes(),
        }
    }

    pub fn iter_mut(&mut self) -> IterMut<Q> {
        IterMut {
            fetches: self.state.map.values_mut(),
            row: ArchetypeRow(0),
            archetypes: self.world.archetypes(),
            _marker: PhantomData,
        }
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
        let (expr, mut state) = Q::init(world, config)?;

        let res = FetcherState::new(world.unsafe_cell_mut(), &expr, state);

        match expr.and(&config.component_access) {
            Ok(new_component_access) => config.component_access = new_component_access,
            Err(_) => {
                return Err(InitError(
                    format!(
                        "`{}` has incompatible component access with previous queries in this \
                         system",
                        any::type_name::<Self>()
                    )
                    .into(),
                ))
            }
        }

        Ok(res)
    }

    unsafe fn get_param<'a>(
        state: &'a mut Self::State,
        system_info: &'a crate::system::SystemInfo,
        event_ptr: crate::event::EventPtr<'a>,
        world: UnsafeWorldCell<'a>,
    ) -> Self::Item<'a> {
        Fetcher { state, world }
    }

    unsafe fn refresh_archetype(
        state: &mut Self::State,
        reason: RefreshArchetypeReason,
        arch: &Archetype,
    ) {
        state.refresh_archetype(reason, arch)
    }
}

impl<Q: Query> fmt::Debug for FetcherState<Q> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("QueryState")
            .field("map", &self.map)
            .field("state", &self.state)
            .finish()
    }
}

pub struct Iter<'a, Q: ReadOnlyQuery> {
    fetches: &'a [Q::Fetch],
    /// Index into the current archetype.
    row: ArchetypeRow,
    archetypes: &'a Archetypes,
}

impl<'a, Q: ReadOnlyQuery> Iterator for Iter<'a, Q> {
    type Item = Q::Item<'a>;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        todo!()
        /*
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
        */
    }
}

pub struct IterMut<'a, Q: Query> {
    // Using a pointer to sneak past a puzzling lifetime problem.
    fetches: *mut [Q::Fetch],
    row: ArchetypeRow,
    archetypes: &'a Archetypes,
    _marker: PhantomData<&'a mut ()>,
}

impl<'a, Q: Query> Iterator for IterMut<'a, Q> {
    type Item = Q::Item<'a>;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        todo!()
        /*
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
        */
    }
}

impl<Q: Query> fmt::Debug for IterMut<'_, Q> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("IterMut")
            .field("fetches", &self.fetches)
            .field("row", &self.row)
            .field("archetypes", &self.archetypes)
            .finish()
    }
}
