use super::*;

macro_rules! impl_query_tuple {
    ($(($Q:ident, $q:ident)),*) => {
        #[allow(unused_variables, clippy::unused_unit)]
        unsafe impl<$($Q: Query),*> Query for ($($Q,)*) {
            type Item<'a> = ($($Q::Item<'a>,)*);

            type ArchState = ($($Q::ArchState,)*);

            type State = ($($Q::State,)*);

            #[allow(unused_mut)]
            fn init(
                world: &mut World,
                config: &mut HandlerConfig
            ) -> Result<(ComponentAccess, Self::State), InitError> {
                let mut filter = ArchetypeFilter::from(true);
                let mut ca = ComponentAccess::new();

                $(
                    let (this_filter, this_ca, $q) = $Q::init(world, config)?;

                    filter = filter.and(&this_filter);
                    ca = ca.and(&this_ca);
                )*

                Ok((filter, ca, ($($q,)*)))
            }

            fn new_state(world: &mut World) -> Self::State {
                (
                    $(
                        $Q::new_state(world),
                    )*
                )
            }

            fn new_arch_state(arch: &Archetype, ($($q,)*): &mut Self::State) -> Option<Self::ArchState> {
                Some((
                    $(
                        $Q::new_arch_state(arch, $q)?,
                    )*
                ))
            }

            unsafe fn get<'a>(($($q,)*): &Self::ArchState, row: ArchetypeRow) -> Self::Item<'a> {
                (
                    $(
                        $Q::get($q, row),
                    )*
                )
            }
        }

        unsafe impl<$($Q: ReadOnlyQuery),*> ReadOnlyQuery for ($($Q,)*) {}
    }
}

// Currently, debug impls for tuples only go up to arity 12.
all_tuples!(impl_query_tuple, 0, 12, Q, q);
