use super::*;

unsafe impl<C: Component> Query for &'_ C {
    type Item<'a> = &'a C;

    type ArchState = ColumnPtr<C>;

    type State = ComponentIdx;

    fn init(
        world: &mut World,
        config: &mut HandlerConfig,
    ) -> Result<(ComponentAccess, Self::State), InitError> {
        let idx = Self::new_state(world);
        let ca = ComponentAccess::var(idx, Access::Read);
        config.referenced_components.insert(idx);

        Ok((ca, idx))
    }

    fn new_state(world: &mut World) -> Self::State {
        world.add_component::<C>().index()
    }

    fn new_arch_state(arch: &Archetype, state: &mut Self::State) -> Option<Self::ArchState> {
        arch.column_of(*state).map(|c| ColumnPtr(c.data().cast()))
    }

    unsafe fn get<'a>(state: &Self::ArchState, row: ArchetypeRow) -> Self::Item<'a> {
        &*state.0.as_ptr().cast_const().add(row.0 as usize)
    }
}

unsafe impl<C: Component> ReadOnlyQuery for &'_ C {}

unsafe impl<C: Component> Query for &'_ mut C {
    type Item<'a> = &'a mut C;

    type ArchState = ColumnPtr<C>;

    type State = ComponentIdx;

    fn init(
        world: &mut World,
        config: &mut HandlerConfig,
    ) -> Result<(ComponentAccess, Self::State), InitError> {
        let () = AssertMutable::<C>::COMPONENT;

        let idx = Self::new_state(world);
        let filter = ArchetypeFilter::var(idx);
        let ca = ComponentAccess::var(idx, Access::ReadWrite);
        config.referenced_components.insert(idx);

        Ok((filter, ca, idx))
    }

    fn new_state(world: &mut World) -> Self::State {
        world.add_component::<C>().index()
    }

    fn new_arch_state(arch: &Archetype, state: &mut Self::State) -> Option<Self::ArchState> {
        <&C>::new_arch_state(arch, state)
    }

    unsafe fn get<'a>(state: &Self::ArchState, row: ArchetypeRow) -> Self::Item<'a> {
        &mut *state.0.as_ptr().add(row.0 as usize)
    }
}

/// Returns the result of `Q` as `Some`, or `None` if `Q` does not match.
unsafe impl<Q: Query> Query for Option<Q> {
    type Item<'a> = Option<Q::Item<'a>>;

    type ArchState = Option<Q::ArchState>;

    type State = Q::State;

    fn init(
        world: &mut World,
        config: &mut HandlerConfig,
    ) -> Result<(ComponentAccess, Self::State), InitError> {
        let (_, ca, state) = Q::init(world, config)?;

        // Access components as `Q` does, but without affecting the matched archetypes.
        Ok((ArchetypeFilter::from(true), ca, state))
    }

    fn new_state(world: &mut World) -> Self::State {
        Q::new_state(world)
    }

    fn new_arch_state(arch: &Archetype, state: &mut Self::State) -> Option<Self::ArchState> {
        Some(Q::new_arch_state(arch, state))
    }

    unsafe fn get<'a>(state: &Self::ArchState, row: ArchetypeRow) -> Self::Item<'a> {
        state.as_ref().map(|f| Q::get(f, row))
    }
}

unsafe impl<Q: ReadOnlyQuery> ReadOnlyQuery for Option<Q> {}

/// Returns the `EntityId` of the matched entity.
unsafe impl Query for EntityId {
    type Item<'a> = Self;

    type ArchState = ColumnPtr<EntityId>;

    type State = ();

    fn init(
        _world: &mut World,
        _config: &mut HandlerConfig,
    ) -> Result<(ComponentAccess, Self::State), InitError> {
        Ok((ArchetypeFilter::from(true), ComponentAccess::new(), ()))
    }

    fn new_state(_world: &mut World) -> Self::State {}

    fn new_arch_state(arch: &Archetype, (): &mut Self::State) -> Option<Self::ArchState> {
        Some(ColumnPtr(unsafe {
            NonNull::new(arch.entity_ids().as_ptr().cast_mut()).unwrap_debug_checked()
        }))
    }

    unsafe fn get<'a>(state: &Self::ArchState, row: ArchetypeRow) -> Self::Item<'a> {
        *state.0.as_ptr().add(row.0 as usize)
    }
}

unsafe impl ReadOnlyQuery for EntityId {}

/// Like `()`, the `PhantomData<T>` query always succeeds.
unsafe impl<T: ?Sized> Query for PhantomData<T> {
    type Item<'a> = Self;

    type ArchState = ();

    type State = ();

    fn init(
        _world: &mut World,
        _config: &mut HandlerConfig,
    ) -> Result<(ComponentAccess, Self::State), InitError> {
        Ok((ArchetypeFilter::from(true), ComponentAccess::new(), ()))
    }

    fn new_state(_world: &mut World) -> Self::State {}

    fn new_arch_state(_arch: &Archetype, _state: &mut Self::State) -> Option<Self::ArchState> {
        Some(())
    }

    unsafe fn get<'a>(_state: &Self::ArchState, _row: ArchetypeRow) -> Self::Item<'a> {
        Self
    }
}

unsafe impl<T: ?Sized> ReadOnlyQuery for PhantomData<T> {}
