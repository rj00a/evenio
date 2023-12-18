use core::fmt;
use std::marker::PhantomData;
use std::{any, mem};

use crate::archetype::{Archetype, ArchetypeIdx, ArchetypeRow, Archetypes};
use crate::debug_checked::{assume_debug_checked, GetDebugChecked, UnwrapDebugChecked};
use crate::entity::EntityId;
use crate::query::{Query, ReadOnlyQuery};
use crate::sparse_map::SparseMap;
use crate::system::{Config, InitError, RefreshArchetypeReason, SystemParam};
use crate::world::{UnsafeWorldCell, World};

pub struct Fetcher<'a, Q: Query> {
    state: &'a mut FetcherState<Q>,
    world: UnsafeWorldCell<'a>,
}

pub struct FetcherState<Q: Query> {
    map: SparseMap<ArchetypeIdx, Q::Fetch>,
    state: Q::State,
}

impl<'a, Q: Query> Fetcher<'a, Q> {
    pub(crate) unsafe fn new(state: &'a mut FetcherState<Q>, world: UnsafeWorldCell<'a>) -> Self {
        Self { state, world }
    }

    pub fn temp(self, entity: EntityId) -> Result<Q::Item<'a>, FetchError> {
        todo!()
    }

    #[inline]
    pub fn get_mut(&mut self, entity: EntityId) -> Result<Q::Item<'_>, FetchError> {
        let entities = self.world.entities();

        let Some(loc) = entities.get(entity) else {
            return Err(FetchError::NoSuchEntity(entity));
        };

        // Eliminate a panicking branch.
        unsafe { assume_debug_checked(loc.archetype != ArchetypeIdx::NULL) };

        // TODO: Resize the sparse array so that all valid archetype indices are in
        // bounds, and then `assume` it. That would eliminate a bounds check.

        let Some(fetch) = self.state.map.get_mut(loc.archetype) else {
            return Err(FetchError::QueryDoesNotMatch(entity));
        };

        Ok(unsafe { Q::fetch_mut(fetch, loc.row) })
    }

    // TODO: get_many_mut

    pub fn iter_mut(&mut self) -> IterMut<Q> {
        IterMut {
            fetches: self.state.map.values_mut(),
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

        // Eliminate a panicking branch.
        unsafe { assume_debug_checked(loc.archetype != ArchetypeIdx::NULL) };

        let Some(fetch) = self.state.map.get(loc.archetype) else {
            return Err(FetchError::QueryDoesNotMatch(entity));
        };

        Ok(unsafe { Q::fetch(fetch, loc.row) })
    }

    pub fn iter(&self) -> Iter<Q> {
        Iter {
            fetches: &self.state.map.values(),
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
        let (expr, mut state) = Q::init(world, config);

        if !config.access.components.is_compatible(&expr) {
            return Err(InitError(
                format!(
                    "`{}` has incompatible component access with other queries in this system",
                    any::type_name::<Self>()
                )
                .into(),
            ));
        }

        config.access.components.and(&expr);

        let sparse_upper_bound = world.archetypes().max_archetype_index().0 + 1;

        let map = SparseMap::new();

        /*
        for (id, arch) in world.archetypes().iter() {
            if let Some(columns) = Q::init_fetch(arch, &mut state) {
                let idx = dense.len() as u32;
                dense.push(Dense { fetch: columns, id });
                *unsafe { sparse.get_debug_checked_mut(id.0 as usize) } = idx;
            }
        }
        */

        Ok(FetcherState { map, state })
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
        idx: ArchetypeIdx,
        arch: &Archetype,
    ) -> bool {
        todo!()

        /*
        match reason {
            RefreshArchetypeReason::New => {
                // Resize the sparse array so that all valid archetype IDs are in bounds.
                if idx.0 >= state.sparse.len() as u32 {
                    state.sparse.resize(idx.0 as usize + 1, u32::MAX);
                }

                debug_assert!(arch.is_empty());

                Q::init_fetch(arch, &mut state.state).is_some()
            }
            RefreshArchetypeReason::InvalidColumns => {
                let idx = *unsafe { state.sparse.get_debug_checked(idx.0 as usize) };

                if idx == u32::MAX {
                    return false;
                }

                let dense = unsafe { state.dense.get_debug_checked_mut(idx as usize) };

                dense.fetch =
                    unsafe { Q::init_fetch(arch, &mut state.state).unwrap_debug_checked() };

                true
            }
            RefreshArchetypeReason::Empty => {
                let idx = mem::replace(
                    unsafe { state.sparse.get_debug_checked_mut(id.0 as usize) },
                    u32::MAX,
                );

                if idx == u32::MAX {
                    return false;
                }

                state.dense.swap_remove(idx as usize);

                if let Some(swapped) = state.dense.get(idx as usize) {
                    *unsafe { state.sparse.get_debug_checked_mut(swapped.id.0 as usize) } = idx;
                }

                true
            }
            RefreshArchetypeReason::Nonempty => {
                if let Some(fetch) = Q::init_fetch(arch, &mut state.state) {
                    let idx = unsafe { state.sparse.get_debug_checked_mut(id.0 as usize) };

                    debug_assert_eq!(*idx, u32::MAX);

                    let new_idx = state.dense.len() as u32;

                    state.dense.push(Dense { fetch, id });

                    *idx = new_idx;

                    true
                } else {
                    false
                }
            }
        }
        */
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
