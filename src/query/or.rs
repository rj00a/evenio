use super::*;

/// A [`Query`] which matches if the `L` or `R` queries match.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum Or<L, R> {
    /// Only the left query matched.
    Left(L),
    /// Only the right query matched.
    Right(R),
    /// Both the left and right queries matched.
    Both(L, R),
}

impl<L, R> Or<L, R> {
    /// Convert `Or<L, R>` to `Or<M, S>` using the supplied closures.
    pub fn map<F, G, M, S>(self, f: F, g: G) -> Or<M, S>
    where
        F: FnOnce(L) -> M,
        G: FnOnce(R) -> S,
    {
        match self {
            Or::Left(l) => Or::Left(f(l)),
            Or::Right(r) => Or::Right(g(r)),
            Or::Both(l, r) => Or::Both(f(l), g(r)),
        }
    }

    /// Converts `&Or<L, R>` to `Or<&L, &R>`.
    pub const fn as_ref(&self) -> Or<&L, &R> {
        match self {
            Or::Left(l) => Or::Left(l),
            Or::Right(r) => Or::Right(r),
            Or::Both(l, r) => Or::Both(l, r),
        }
    }

    /// Converts `&mut Or<L, R>` to `Or<&mut L, &mut R>`.
    pub fn as_mut(&mut self) -> Or<&mut L, &mut R> {
        match self {
            Or::Left(l) => Or::Left(l),
            Or::Right(r) => Or::Right(r),
            Or::Both(l, r) => Or::Both(l, r),
        }
    }
}

unsafe impl<L, R> Query for Or<L, R>
where
    L: Query,
    R: Query,
{
    type Item<'a> = Or<L::Item<'a>, R::Item<'a>>;

    type ArchState = Or<L::ArchState, R::ArchState>;

    type State = (L::State, R::State);

    fn init(
        world: &mut World,
        config: &mut Config,
    ) -> Result<(ArchetypeFilter, ComponentAccess, Self::State), InitError> {
        let (filter_lhs, ca_lhs, state_lhs) = L::init(world, config)?;
        let (filter_rhs, ca_rhs, state_rhs) = R::init(world, config)?;

        Ok((
            filter_lhs.or(&filter_rhs),
            ca_lhs.or(&ca_rhs),
            (state_lhs, state_rhs),
        ))
    }

    fn new_state(world: &mut World) -> Self::State {
        (L::new_state(world), R::new_state(world))
    }

    fn new_arch_state(
        arch: &Archetype,
        (left_state, right_state): &mut Self::State,
    ) -> Option<Self::ArchState> {
        match (
            L::new_arch_state(arch, left_state),
            R::new_arch_state(arch, right_state),
        ) {
            (None, None) => None,
            (None, Some(r)) => Some(Or::Right(r)),
            (Some(l), None) => Some(Or::Left(l)),
            (Some(l), Some(r)) => Some(Or::Both(l, r)),
        }
    }

    unsafe fn get<'a>(state: &Self::ArchState, row: ArchetypeRow) -> Self::Item<'a> {
        state.as_ref().map(|l| L::get(l, row), |r| R::get(r, row))
    }
}

unsafe impl<L, R> ReadOnlyQuery for Or<L, R>
where
    L: ReadOnlyQuery,
    R: ReadOnlyQuery,
{
}
