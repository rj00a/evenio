use super::*;

/// A [`Query`] which matches if query `Q` doesn't match.
pub struct Not<Q>(PhantomData<fn() -> Q>);

impl<Q> Not<Q> {
    /// Create a new instance.
    pub const fn new() -> Self {
        Self(PhantomData)
    }
}

impl<Q> Clone for Not<Q> {
    fn clone(&self) -> Self {
        *self
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

    type ArchState = ();

    type State = Q::State;

    fn init(
        world: &mut World,
        config: &mut HandlerConfig,
    ) -> Result<(ComponentAccess, Self::State), InitError> {
        let (filter, ca, state) = Q::init(world, config)?;

        Ok((filter.not(), ca.not(), state))
    }

    fn new_state(world: &mut World) -> Self::State {
        Q::new_state(world)
    }

    fn new_arch_state(arch: &Archetype, state: &mut Self::State) -> Option<Self::ArchState> {
        match Q::new_arch_state(arch, state) {
            Some(_) => None,
            None => Some(()),
        }
    }

    unsafe fn get<'a>(_state: &Self::ArchState, _row: ArchetypeRow) -> Self::Item<'a> {
        Not::new()
    }
}

unsafe impl<Q: Query> ReadOnlyQuery for Not<Q> {}
