use super::*;

/// A [`Query`] which matches if the `L` or `R` queries match, but not both.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum Xor<L, R> {
    /// Only the left query matched.
    Left(L),
    /// Only the right query matched.
    Right(R),
}

impl<L, R> Xor<L, R> {
    /// Convert `Xor<L, R>` to `Xor<M, S>` using the supplied closures.
    pub fn map<F, G, M, S>(self, f: F, g: G) -> Xor<M, S>
    where
        F: FnOnce(L) -> M,
        G: FnOnce(R) -> S,
    {
        match self {
            Xor::Left(l) => Xor::Left(f(l)),
            Xor::Right(r) => Xor::Right(g(r)),
        }
    }

    /// Convert `&Xor<L, R>` to `Xor<&L, &R>`.
    pub const fn as_ref(&self) -> Xor<&L, &R> {
        match self {
            Xor::Left(l) => Xor::Left(l),
            Xor::Right(r) => Xor::Right(r),
        }
    }

    /// Convert `&mut Xor<L, R>` to `Xor<&mut L, &mut R>`.
    pub fn as_mut(&mut self) -> Xor<&mut L, &mut R> {
        match self {
            Xor::Left(l) => Xor::Left(l),
            Xor::Right(r) => Xor::Right(r),
        }
    }
}

unsafe impl<L, R> Query for Xor<L, R>
where
    L: Query,
    R: Query,
{
    type Item<'a> = Xor<L::Item<'a>, R::Item<'a>>;

    type ArchState = Xor<L::ArchState, R::ArchState>;

    type State = (L::State, R::State);

    fn init(
        world: &mut World,
        config: &mut HandlerConfig,
    ) -> Result<(ComponentAccess, Self::State), InitError> {
        let (filter_lhs, ca_lhs, state_lhs) = L::init(world, config)?;
        let (filter_rhs, ca_rhs, state_rhs) = R::init(world, config)?;

        Ok((
            filter_lhs.xor(&filter_rhs),
            ca_lhs.xor(&ca_rhs),
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
            (None, Some(r)) => Some(Xor::Right(r)),
            (Some(l), None) => Some(Xor::Left(l)),
            (Some(_), Some(_)) => None,
        }
    }

    unsafe fn get<'a>(state: &Self::ArchState, row: ArchetypeRow) -> Self::Item<'a> {
        state.as_ref().map(|l| L::get(l, row), |r| R::get(r, row))
    }
}

unsafe impl<L, R> ReadOnlyQuery for Xor<L, R>
where
    L: ReadOnlyQuery,
    R: ReadOnlyQuery,
{
}
