/// A [`Query`] which matches if query `Q` matches.
///
/// Unlike `Q` however, `With<Q>` does not provide access to the data returned
/// by `Q`. This is useful for avoiding access conflicts.
///
/// Example: `&C` requires read-only access to component `C`, but `With<&C>`
/// does not require access at all.
pub struct With<Q>(PhantomData<fn() -> Q>);

impl<Q> With<Q> {
    /// Create a new instance.
    pub const fn new() -> Self {
        Self(PhantomData)
    }
}

impl<Q> Clone for With<Q> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<Q> Copy for With<Q> {}

impl<Q> Default for With<Q> {
    fn default() -> Self {
        Self(Default::default())
    }
}

impl<Q> fmt::Debug for With<Q> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("With").finish()
    }
}

unsafe impl<Q: Query> Query for With<Q> {
    type Item<'a> = Self;

    type ArchState = ();

    type State = Q::State;

    fn init(
        world: &mut World,
        config: &mut HandlerConfig,
    ) -> Result<(ArchetypeFilter, ComponentAccess, Self::State), InitError> {
        let (filter, mut ca, state) = Q::init(world, config)?;

        ca.clear_access();

        Ok((filter, ca, state))
    }

    fn new_state(world: &mut World) -> Self::State {
        Q::new_state(world)
    }

    fn new_arch_state(arch: &Archetype, state: &mut Self::State) -> Option<Self::ArchState> {
        Q::new_arch_state(arch, state).map(|_| ())
    }

    unsafe fn get<'a>(_state: &Self::ArchState, _row: ArchetypeRow) -> Self::Item<'a> {
        With::new()
    }
}

unsafe impl<Q: Query> ReadOnlyQuery for With<Q> {}
