use core::fmt;

use super::Iter;
use crate::archetype::{Archetype, ArchetypeIdx, Archetypes};
use crate::assert::assume_debug_checked;
use crate::entity::{Entities, EntityId, EntityLocation};
use crate::event::EventPtr;
use crate::handler::{HandlerConfig, HandlerInfo, HandlerParam, InitError};
use crate::prelude::ReadOnlyQuery;
use crate::query::Query;
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
        let (filter, ca, state) = Q::init(world, config)?;

        config.archetype_filter = config.archetype_filter.or(&filter);
        config.component_access = config.component_access.and(&ca);

        let res = FetcherState::new(state);

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
            return Err(FetchError::NoSuchEntity);
        };

        // Eliminate a branch in `SparseMap::get`.
        assume_debug_checked(loc.archetype != ArchetypeIdx::NULL);

        let Some(state) = self.map.get(loc.archetype) else {
            return Err(FetchError::QueryDoesNotMatch);
        };

        Ok(Q::get(state, loc.row))
    }

    #[inline]
    pub(crate) unsafe fn get_mut(
        &mut self,
        entities: &Entities,
        entity: EntityId,
    ) -> Result<Q::Item<'_>, FetchError> {
        let Some(loc) = entities.get(entity) else {
            return Err(FetchError::NoSuchEntity);
        };

        // Eliminate a panicking branch.
        assume_debug_checked(loc.archetype != ArchetypeIdx::NULL);

        // TODO: Resize the sparse array so that all valid archetype indices are in
        // bounds, and then `assume` it. That would eliminate a bounds check?

        let Some(state) = self.map.get_mut(loc.archetype) else {
            return Err(FetchError::QueryDoesNotMatch);
        };

        Ok(Q::get(state, loc.row))
    }

    #[inline]
    pub(crate) unsafe fn get_by_location_mut(&mut self, loc: EntityLocation) -> Q::Item<'_> {
        let state = self
            .map
            .get(loc.archetype)
            .expect_debug_checked("invalid entity location");
        
        Q::get(state, loc.row)
    }

    // TODO: get_many_mut

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
        Iter::new(self.map.keys(), self.map.values(), archetypes)
    }

    #[cfg(feature = "rayon")]
    pub(crate) unsafe fn par_iter<'a>(&'a self, archetypes: &'a Archetypes) -> ParIter<'a, Q>
    where
        Q: ReadOnlyQuery,
    {
        ParIter::new(self.map.keys(), self.map.values(), archetypes)
    }

    #[cfg(feature = "rayon")]
    pub(crate) unsafe fn par_iter_mut<'a>(
        &'a mut self,
        archetypes: &'a Archetypes,
    ) -> ParIter<'a, Q> {
        ParIter::new(self.map.keys(), self.map.values(), archetypes)
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
    pub fn get(&self, entity: EntityId) -> Result<Q::Item<'_>, FetchError>
    where
        Q: ReadOnlyQuery,
    {
        unsafe { self.state.get(self.world.entities(), entity) }
    }

    /// Returns the query item for the given entity.
    ///
    /// If the entity doesn't exist or doesn't match the query, then a
    /// [`GetError`] is returned.
    #[inline]
    pub fn get_mut(&mut self, entity: EntityId) -> Result<Q::Item<'_>, FetchError> {
        unsafe { self.state.get_mut(self.world.entities(), entity) }
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
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum FetchError {
    /// Entity does not exist.
    NoSuchEntity,
    /// Entity does not match the query.
    QueryDoesNotMatch,
    /// Entity was requested mutably more than once.
    AliasedMutability,
}

impl fmt::Display for FetchError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FetchError::NoSuchEntity => write!(f, "entity does not exist"),
            FetchError::QueryDoesNotMatch => write!(f, "entity does not match the query"),
            FetchError::AliasedMutability => {
                write!(f, "entity was requested mutably more than once")
            }
        }
    }
}

#[cfg(feature = "std")]
#[cfg_attr(docsrs, doc(cfg(feature = "std")))]
impl std::error::Error for FetchError {}

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
