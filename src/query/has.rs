/// A [`Query`] which returns a boolean indicating whether the query `Q`
/// matches.
///
/// Like [`With`], `Has` does not provide access to the data returned by `Q`.
pub struct Has<Q> {
    has: bool,
    _marker: PhantomData<fn() -> Q>,
}

impl<Q> Has<Q> {
    /// Creates a new instance wrapping `has`.
    pub const fn new(has: bool) -> Self {
        Self {
            has,
            _marker: PhantomData,
        }
    }

    /// Extracts the inner boolean.
    pub const fn get(self) -> bool {
        self.has
    }
}

impl<Q> Clone for Has<Q> {
    fn clone(&self) -> Self {
        *self
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

unsafe impl<Q: Query> Query for Has<Q> {
    type Item<'a> = Self;

    type ArchState = bool;

    type State = Q::State;

    fn init(
        world: &mut World,
        config: &mut HandlerConfig,
    ) -> Result<(ArchetypeFilter, ComponentAccess, Self::State), InitError> {
        let (_, _, state) = Q::init(world, config)?;

        Ok((ArchetypeFilter::from(true), ComponentAccess::new(), state))
    }

    fn new_state(world: &mut World) -> Self::State {
        Q::new_state(world)
    }

    fn new_arch_state(arch: &Archetype, state: &mut Self::State) -> Option<Self::ArchState> {
        Some(Q::new_arch_state(arch, state).is_some())
    }

    unsafe fn get<'a>(state: &Self::ArchState, _row: ArchetypeRow) -> Self::Item<'a> {
        Self::new(*state)
    }
}

unsafe impl<Q: Query> ReadOnlyQuery for Has<Q> {}
