//! Accessing components on entities.

use core::iter::FusedIterator;
use core::mem::{self, MaybeUninit};
use core::ops::{Deref, DerefMut};
use core::panic::{RefUnwindSafe, UnwindSafe};
use core::ptr::NonNull;
use core::{any, fmt};

use crate::archetype::{Archetype, ArchetypeIdx, ArchetypeRow, Archetypes};
use crate::assume_unchecked;
use crate::entity::{Entities, EntityId, EntityLocation};
use crate::event::EventPtr;
use crate::handler::{HandlerConfig, HandlerInfo, HandlerParam, InitError};
use crate::query::{Query, ReadOnlyQuery};
use crate::sparse_map::SparseMap;
use crate::world::{UnsafeWorldCell, World};

/// Internal state for a [`Fetcher`].
#[doc(hidden)]
pub struct FetcherState<Q: Query> {
    /// Stores the query's per-archetype state.
    map: SparseMap<ArchetypeIdx, Q::ArchState>,
    /// Stores the query's overall state.
    state: Q::State,
}

impl<Q: Query> FetcherState<Q> {
    /// Constructs a new fetcher state from the given query state.
    pub(crate) fn new(state: Q::State) -> Self {
        Self {
            map: SparseMap::new(),
            state,
        }
    }

    /// Initializes the fetcher state with a world and handler config. This
    /// also initializes the query.
    pub(crate) fn init(world: &mut World, config: &mut HandlerConfig) -> Result<Self, InitError> {
        let (ca, state) = Q::init(world, config)?;

        config.push_component_access(ca);

        Ok(FetcherState::new(state))
    }

    /// Execute the query for an entity. Returns the query's result or a
    /// [`GetError`] if there is no entity with the given id or the query does
    /// not match the entity's archetype.
    ///
    /// # Safety
    ///
    /// - The location of the passed entity must be valid.
    /// - Must have permission to access the components that the query accesses.
    #[inline]
    pub(crate) unsafe fn get_unchecked(
        &self,
        entities: &Entities,
        entity: EntityId,
    ) -> Result<Q::This<'_>, GetError> {
        let Some(loc) = entities.get(entity) else {
            return Err(GetError::NoSuchEntity);
        };

        // Eliminate a branch in `SparseMap::get`.
        // SAFETY: Caller guarantees the location is valid.
        assume_unchecked(loc.archetype != ArchetypeIdx::NULL);

        let Some(state) = self.map.get(loc.archetype) else {
            return Err(GetError::QueryDoesNotMatch);
        };

        Ok(Q::get(state, loc.row))
    }

    /// Execute the query for `N` entities at once. Returns the queries' results
    /// or a [`GetManyMutError`] if any entity id is invalid or if there is any
    /// entity whose archetype is not matched by the query.
    ///
    /// # Safety
    ///
    /// - The locations of the passed entities must be valid.
    /// - Must have permission to access the components that the query accesses.
    #[inline]
    pub(crate) unsafe fn get_many_mut<const N: usize>(
        &mut self,
        entities: &Entities,
        array: [EntityId; N],
    ) -> Result<[Q::This<'_>; N], GetManyMutError> {
        // Check for overlapping entity ids.
        for i in 0..N {
            for j in 0..i {
                if array[i] == array[j] {
                    return Err(GetManyMutError::AliasedMutability);
                }
            }
        }

        // Create an uninitialized array for the query results.
        // TODO: optimize array creation like so:
        //  `[const { MaybeUninit::<Q::This<'_>>::uninit() }; N]`
        let mut res: [MaybeUninit<Q::This<'_>>; N] = [(); N].map(|()| MaybeUninit::uninit());

        for i in 0..N {
            // Execute the query for a single entity.
            match self.get_unchecked(entities, array[i]) {
                Ok(item) => {
                    // Move the query result into the array.
                    res[i] = MaybeUninit::new(item)
                }
                Err(e) => {
                    // Drop all already collected query results.
                    if mem::needs_drop::<Q::This<'_>>() {
                        for item in res.iter_mut().take(i) {
                            item.assume_init_drop();
                        }
                    }

                    // Propagate the error to the caller.
                    return Err(e.into());
                }
            }
        }

        // Return the collected results.
        // SAFETY: Every element in the array has been initialized above.
        Ok(mem::transmute_copy::<
            [MaybeUninit<Q::This<'_>>; N],
            [Q::This<'_>; N],
        >(&res))
    }

    /// Executes the query for an entity using its location.
    ///
    /// # Safety
    ///
    /// - The entity location must be valid.
    /// - Must have permission to access the components that the query accesses.
    #[inline]
    pub(crate) unsafe fn get_by_location_mut(&mut self, loc: EntityLocation) -> Q::This<'_> {
        // SAFETY: Caller ensures location is valid.
        let state = self.map.get(loc.archetype).unwrap_unchecked();
        Q::get(state, loc.row)
    }

    /// Returns an iterator over the results of this query for each entity in an
    /// archetype that the query matches.
    ///
    /// # Safety
    ///
    /// Must have permission to access the components that the query accesses.
    #[inline]
    pub(crate) unsafe fn iter<'a>(&'a self, archetypes: &'a Archetypes) -> Iter<'a, Q>
    where
        Q: ReadOnlyQuery,
    {
        self.iter_unchecked(archetypes)
    }

    /// Returns an iterator over the results of this query for each entity in an
    /// archetype that the query matches.
    ///
    /// # Safety
    ///
    /// Must have permission to access the components that the query accesses.
    #[inline]
    pub(crate) unsafe fn iter_mut<'a>(&'a mut self, archetypes: &'a Archetypes) -> Iter<'a, Q> {
        self.iter_unchecked(archetypes)
    }

    /// Returns an iterator over the results of this query for each entity in an
    /// archetype that the query matches.
    ///
    /// # Safety
    ///
    /// Must have permission to access the components that the query accesses.
    unsafe fn iter_unchecked<'a>(&'a self, archetypes: &'a Archetypes) -> Iter<'a, Q> {
        let indices = self.map.keys();
        let states = self.map.values();

        assume_unchecked(indices.len() == states.len());

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
                state: NonNull::new(start).unwrap_unchecked(),
                state_last: NonNull::new(end).unwrap_unchecked(),
                index: NonNull::new(indices.as_ptr().cast_mut()).unwrap_unchecked(),
                row: ArchetypeRow(0),
                len: archetypes.get(indices[0]).unwrap_unchecked().entity_count(),
                archetypes,
            }
        }
    }

    /// Returns a parallel iterator over the results of this query for each
    /// entity in an archetype that the query matches.
    ///
    /// # Safety
    ///
    /// Must have permission to access the components that the query accesses.
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

    /// Returns a parallel iterator over the results of this query for each
    /// entity in an archetype that the query matches.
    ///
    /// # Safety
    ///
    /// Must have permission to access the components that the query accesses.
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

    /// Refreshes the query's archetype state for the given archetype.
    pub(crate) fn refresh_archetype(&mut self, arch: &Archetype) {
        debug_assert!(
            arch.entity_count() != 0,
            "`refresh_archetype` called with empty archetype"
        );

        if let Some(fetch) = Q::new_arch_state(arch, &mut self.state) {
            self.map.insert(arch.index(), fetch);
        }
    }

    /// Removes the query's archetype state for the given archetype.
    pub(crate) fn remove_archetype(&mut self, arch: &Archetype) {
        self.map.remove(arch.index());
    }
}

unsafe impl<'a, Q> Send for Fetcher<'a, Q>
where
    Q: Query,
    Q::This<'a>: Send,
{
}

unsafe impl<'a, Q> Sync for Fetcher<'a, Q>
where
    Q: Query,
    Q::This<'a>: Sync,
{
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
/// chapter](crate::tutorial#fetching).
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
    pub fn get(&self, entity: EntityId) -> Result<Q::This<'_>, GetError>
    where
        Q: ReadOnlyQuery,
    {
        unsafe { self.state.get_unchecked(self.world.entities(), entity) }
    }

    /// Returns the query item for the given entity without checking borrowing
    /// rules.
    ///
    /// This is useful when you know the entity IDs are disjoint but can't prove
    /// it to the compiler.
    ///
    /// If the entity doesn't exist or doesn't match the query, then a
    /// [`GetError`] is returned.
    ///
    /// # Safety
    ///
    /// You must ensure that all entities that co-occur are disjoint if they
    /// contain any mutable references.
    pub unsafe fn get_unchecked(&self, entity: EntityId) -> Result<Q::This<'_>, GetError> {
        self.state.get_unchecked(self.world.entities(), entity)
    }

    /// Returns the query item for the given entity.
    ///
    /// If the entity doesn't exist or doesn't match the query, then a
    /// [`GetError`] is returned.
    #[inline]
    pub fn get_mut(&mut self, entity: EntityId) -> Result<Q::This<'_>, GetError> {
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
    ) -> Result<[Q::This<'_>; N], GetManyMutError> {
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
    type Item = Q::This<'a>;

    type IntoIter = Iter<'a, Q>;

    fn into_iter(self) -> Self::IntoIter {
        unsafe { self.state.iter_mut(self.world.archetypes()) }
    }
}

impl<'a, Q: ReadOnlyQuery> IntoIterator for &'a Fetcher<'_, Q> {
    type Item = Q::This<'a>;

    type IntoIter = Iter<'a, Q>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl<'a, Q: Query> IntoIterator for &'a mut Fetcher<'_, Q> {
    type Item = Q::This<'a>;

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

    type This<'a> = Fetcher<'a, Q>;

    fn init(world: &mut World, config: &mut HandlerConfig) -> Result<Self::State, InitError> {
        FetcherState::init(world, config)
    }

    unsafe fn get<'a>(
        state: &'a mut Self::State,
        _info: &'a HandlerInfo,
        _event_ptr: EventPtr<'a>,
        _target_location: EntityLocation,
        world: UnsafeWorldCell<'a>,
    ) -> Self::This<'a> {
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
/// # #[derive(GlobalEvent)] struct E;
/// use evenio::prelude::*;
///
/// #[derive(Component, Debug)]
/// struct MyComponent(i32);
///
/// let mut world = World::new();
///
/// world.add_handler(|_: Receiver<E>, data: Single<&MyComponent>| {
///     println!("The data is: {data:?}");
/// });
///
/// let e = world.spawn();
/// world.insert(e, MyComponent(123));
///
/// world.send(E);
/// ```
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Default, Debug)]
pub struct Single<Q>(Q);

impl<Q> Single<Q> {
    /// Create a new instance of `Single`.
    pub const fn new(q: Q) -> Self {
        Self(q)
    }

    /// Consumes the `Single` and returns the inner value.
    ///
    /// # Examples
    ///
    /// ```
    /// use evenio::fetch::Single;
    ///
    /// let single = Single::new("bananas");
    /// assert_eq!(Single::into_inner(single), "bananas");
    /// ```
    pub fn into_inner(this: Self) -> Q {
        this.0
    }
}

unsafe impl<Q: Query + 'static> HandlerParam for Single<Q> {
    type State = FetcherState<Q>;

    type This<'a> = Single<Q::This<'a>>;

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
    ) -> Self::This<'a> {
        match TrySingle::get(state, info, event_ptr, target_location, world) {
            Ok(item) => Single(item),
            Err(e) => {
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

impl<'a, T> Deref for Single<&'a T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.0
    }
}

impl<'a, T> Deref for Single<&'a mut T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.0
    }
}

impl<'a, T> DerefMut for Single<&'a mut T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.0
    }
}

impl<'a, T> AsRef<T> for Single<&'a T> {
    fn as_ref(&self) -> &T {
        self.0
    }
}

impl<'a, T> AsRef<T> for Single<&'a mut T> {
    fn as_ref(&self) -> &T {
        self.0
    }
}

impl<'a, T> AsMut<T> for Single<&'a mut T> {
    fn as_mut(&mut self) -> &mut T {
        self.0
    }
}

impl<T> From<T> for Single<T> {
    fn from(value: T) -> Self {
        Self(value)
    }
}

/// Like [`Single`], but yields a `Result` rather than panicking on error.
///
/// This is useful if you need to explicitly handle the situation where the
/// query does not match exactly one entity.
pub type TrySingle<Q> = Result<Q, SingleError>;

unsafe impl<Q: Query + 'static> HandlerParam for TrySingle<Q> {
    type State = FetcherState<Q>;

    type This<'a> = Result<Q::This<'a>, SingleError>;

    fn init(world: &mut World, config: &mut HandlerConfig) -> Result<Self::State, InitError> {
        FetcherState::init(world, config)
    }

    unsafe fn get<'a>(
        state: &'a mut Self::State,
        _info: &'a HandlerInfo,
        _event_ptr: EventPtr<'a>,
        _target_location: EntityLocation,
        world: UnsafeWorldCell<'a>,
    ) -> Self::This<'a> {
        let mut it = state.iter_mut(world.archetypes());

        let Some(item) = it.next() else {
            return Err(SingleError::QueryDoesNotMatch);
        };

        if it.next().is_some() {
            return Err(SingleError::MoreThanOneMatch);
        }

        Ok(item)
    }

    fn refresh_archetype(state: &mut Self::State, arch: &Archetype) {
        state.refresh_archetype(arch)
    }

    fn remove_archetype(state: &mut Self::State, arch: &Archetype) {
        state.remove_archetype(arch)
    }
}

/// Error raised when fetching exactly one entity matching a query fails.
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
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
    type Item = Q::This<'a>;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        // If we reached the end of the current archetype, move on to the next
        // or return `None`.
        if self.row.0 == self.len {
            if self.state == self.state_last {
                // There are no more archetypes to iterate through.
                return None;
            }

            // Move on to the next archetype.
            self.state = unsafe { NonNull::new_unchecked(self.state.as_ptr().add(1)) };
            self.index = unsafe { NonNull::new_unchecked(self.index.as_ptr().add(1)) };

            let idx = unsafe { *self.index.as_ptr() };
            let arch = unsafe { self.archetypes.get(idx).unwrap_unchecked() };

            self.row = ArchetypeRow(0);
            self.len = arch.entity_count();

            // SAFETY: Fetcher state only contains nonempty archetypes.
            unsafe { assume_unchecked(self.len > 0) };
        }

        // Execute the query for the current entity.
        let state = unsafe { &*self.state.as_ptr().cast_const() };
        let item = unsafe { Q::get(state, self.row) };

        // Move on to the next row (entity) in the archetype.
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
        // The number of rows in the current archetype which we did not iterate
        // over yet.
        let mut remaining = self.len - self.row.0;

        let mut index = self.index.as_ptr();

        // The index of the last archetype we will iterate through.
        let index_last = unsafe {
            // TODO: use `.sub_ptr` when stabilized.
            index.add(self.state_last.as_ptr().offset_from(self.state.as_ptr()) as usize)
        };

        // Add the number of rows of each remaining archetype to `remaining`.
        while index != index_last {
            index = unsafe { index.add(1) };

            remaining += unsafe { self.archetypes.get(*index).unwrap_unchecked() }.entity_count();
        }

        // Return the total.
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

unsafe impl<'a, Q> Send for Iter<'_, Q>
where
    Q: Query,
    Q::This<'a>: Send,
{
}

unsafe impl<'a, Q> Sync for Iter<'a, Q>
where
    Q: Query,
    Q::This<'a>: Sync,
{
}

impl<'a, Q> UnwindSafe for Iter<'a, Q>
where
    Q: Query,
    Q::This<'a>: UnwindSafe,
{
}

impl<'a, Q> RefUnwindSafe for Iter<'a, Q>
where
    Q: Query,
    Q::This<'a>: RefUnwindSafe,
{
}

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
        Q::This<'a>: Send,
    {
        type Item = Q::This<'a>;

        fn drive_unindexed<C>(self, consumer: C) -> C::Result
        where
            C: UnindexedConsumer<Self::Item>,
        {
            unsafe { assume_unchecked(self.arch_states.len() == self.arch_indices.len()) };

            self.arch_states
                .par_iter()
                .zip_eq(self.arch_indices)
                .flat_map(|(state, &index)| {
                    let entity_count =
                        unsafe { self.archetypes.get(index).unwrap_unchecked() }.entity_count();

                    (0..entity_count).into_par_iter().map(|row| {
                        let item: Q::This<'a> = unsafe { Q::get(state, ArchetypeRow(row)) };
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
        Q::This<'a>: Send,
    {
        type Iter = ParIter<'a, Q>;

        type Item = Q::This<'a>;

        fn into_par_iter(self) -> Self::Iter {
            unsafe { self.state.par_iter_mut(self.world.archetypes()) }
        }
    }

    #[cfg_attr(docsrs, doc(cfg(feature = "rayon")))]
    impl<'a, Q> IntoParallelIterator for &'a Fetcher<'_, Q>
    where
        Q: ReadOnlyQuery,
        Q::This<'a>: Send,
    {
        type Iter = ParIter<'a, Q>;

        type Item = Q::This<'a>;

        fn into_par_iter(self) -> Self::Iter {
            unsafe { self.state.par_iter(self.world.archetypes()) }
        }
    }

    #[cfg_attr(docsrs, doc(cfg(feature = "rayon")))]
    impl<'a, Q: Query> IntoParallelIterator for &'a mut Fetcher<'_, Q>
    where
        Q: Query,
        Q::This<'a>: Send,
    {
        type Iter = ParIter<'a, Q>;

        type Item = Q::This<'a>;

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

    #[derive(GlobalEvent)]
    struct E1;

    #[derive(GlobalEvent)]
    struct E2;

    #[derive(GlobalEvent)]
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
    fn fetch_many_mut() {
        let mut world = World::new();

        let e1 = world.spawn();
        world.insert(e1, C1(123));
        let e2 = world.spawn();
        world.insert(e2, C1(456));
        let e3 = world.spawn();
        world.insert(e3, C1(789));

        world.add_handler(move |_: Receiver<E1>, mut f: Fetcher<&mut C1>| {
            assert_eq!(
                f.get_many_mut([e1, e2, e3]),
                Ok([&mut C1(123), &mut C1(456), &mut C1(789)])
            );
        });

        world.send(E1);
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
                assert_eq!(s1, Err(SingleError::QueryDoesNotMatch));
                assert_eq!(s2, Ok(&C2(123)));
                assert_eq!(s3, Err(SingleError::MoreThanOneMatch));
            },
        );

        world.send(E1);
    }

    fn _assert_auto_trait_impls()
    where
        Fetcher<'static, ()>: Send + Sync,
        Fetcher<'static, &'static C1>: Send + Sync,
        Fetcher<'static, (&'static C1, &'static mut C2)>: Send + Sync,
    {
    }
}
