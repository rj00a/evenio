//! Type-level DSL for retrieving data from entities.

use core::fmt;
use core::marker::PhantomData;
use core::ptr::NonNull;

use evenio_macros::all_tuples;
pub use evenio_macros::Query;

use crate::access::{Access, ComponentAccess};
use crate::archetype::{Archetype, ArchetypeRow};
use crate::assert::{AssertMutable, UnwrapDebugChecked};
use crate::component::{Component, ComponentIdx};
use crate::entity::EntityId;
use crate::handler::{HandlerConfig, InitError};
use crate::world::World;

/// Types that can be fetched from an entity.
///
/// For more information, see the relevant [tutorial
/// chapter](crate::tutorial::ch05_fetching).
///
/// # Deriving
///
/// This trait can be safely implemented using the `Query` derive macro. For a
/// struct to derive `Query`, all fields must also implement `Query`.
///
/// ```
/// # #[derive(Event)]
/// # struct MyEvent;
/// use evenio::prelude::*;
///
/// #[derive(Component, Debug)]
/// struct A(i32);
///
/// #[derive(Component, Debug)]
/// struct B;
///
/// #[derive(Query, Debug)]
/// struct CustomQuery<'a> {
///     foo: &'a mut A,
///     bar: EntityId,
///     baz: Has<&'static B>,
/// }
///
/// let mut world = World::new();
///
/// world.add_handler(|_: Receiver<MyEvent>, q: Fetcher<CustomQuery>| {
///     for item in q {
///         println!("{item:?}");
///     }
/// });
///
/// world.send(MyEvent);
/// ```
///
/// # Safety
///
/// Implementors must ensure that [`Query::init`] correctly registers the data
/// accessed in [`Query::get`].
pub unsafe trait Query {
    /// The item returned by this query. This is usually the same type as
    /// `Self`, but with a modified lifetime.
    type Item<'a>;
    /// Per-archetype state.
    type ArchState: Send + Sync + fmt::Debug + 'static;
    /// Cached data for fetch initialization.
    type State: Send + Sync + fmt::Debug + 'static;

    /// Initialize the query. Returns an expression describing the components
    /// accessed by the query and a new instance of [`Self::State`].
    fn init(
        world: &mut World,
        config: &mut HandlerConfig,
    ) -> Result<(ComponentAccess, Self::State), InitError>;

    /// Returns a new [`Self::State`] instance.
    fn new_state(world: &mut World) -> Self::State;

    /// Returns a new [`Self::ArchState`] instance.
    fn new_arch_state(arch: &Archetype, state: &mut Self::State) -> Option<Self::ArchState>;

    /// Gets the query item at the given row in the archetype.
    ///
    /// # Safety
    /// - `row` must be in bounds.
    /// - Must have the appropriate component access permissions described by
    ///   the [`ComponentAccess`] returned by [`init`].
    /// - The lifetime of the item is chosen by the caller. The item must not
    ///   outlive the data it references.
    ///
    /// [`init`]: Self::init
    unsafe fn get<'a>(state: &Self::ArchState, row: ArchetypeRow) -> Self::Item<'a>;
}

/// Marker trait for queries which do not access data mutably.
///
/// For instance, the query `(&A, &B)` is read-only, but `(&A, &mut B)` is not.
///
/// # Safety
///
/// The query must not access data mutably, as methods like [`Fetcher::get`]
/// rely on this property to avoid mutable aliasing.
///
/// [`Fetcher::get`]: crate::fetch::Fetcher::get
pub unsafe trait ReadOnlyQuery: Query {}

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
        let ca = ComponentAccess::var(idx, Access::ReadWrite);
        config.referenced_components.insert(idx);

        Ok((ca, idx))
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
                let mut ca = ComponentAccess::new_true();

                $(
                    let (this_ca, $q) = $Q::init(world, config)?;
                    ca = ca.and(&this_ca);
                )*

                Ok((ca, ($($q,)*)))
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

/// Returns the result of `Q` as `Some`, or `None` if `Q` does not match.
unsafe impl<Q: Query> Query for Option<Q> {
    type Item<'a> = Option<Q::Item<'a>>;

    type ArchState = Option<Q::ArchState>;

    type State = Q::State;

    fn init(
        world: &mut World,
        config: &mut HandlerConfig,
    ) -> Result<(ComponentAccess, Self::State), InitError> {
        let (ca, state) = Q::init(world, config)?;
        Ok((ComponentAccess::new_true().or(&ca), state))
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
        config: &mut HandlerConfig,
    ) -> Result<(ComponentAccess, Self::State), InitError> {
        let (ca_lhs, state_lhs) = L::init(world, config)?;
        let (ca_rhs, state_rhs) = R::init(world, config)?;

        let ca_both = ca_lhs.and(&ca_rhs);

        Ok((ca_lhs.or(&ca_rhs).or(&ca_both), (state_lhs, state_rhs)))
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
        let (ca_lhs, state_lhs) = L::init(world, config)?;
        let (ca_rhs, state_rhs) = R::init(world, config)?;

        Ok((
            dbg!(ca_lhs.and(&ca_rhs.not()).or(&ca_rhs.and(&ca_lhs.not()))),
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
        let (ca, state) = Q::init(world, config)?;

        Ok((ca.not(), state))
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
    ) -> Result<(ComponentAccess, Self::State), InitError> {
        let (mut ca, state) = Q::init(world, config)?;

        ca.clear_access();

        Ok((ca, state))
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
    ) -> Result<(ComponentAccess, Self::State), InitError> {
        let (_, state) = Q::init(world, config)?;

        Ok((ComponentAccess::new_true(), state))
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

/// Returns the `EntityId` of the matched entity.
unsafe impl Query for EntityId {
    type Item<'a> = Self;

    type ArchState = ColumnPtr<EntityId>;

    type State = ();

    fn init(
        _world: &mut World,
        _config: &mut HandlerConfig,
    ) -> Result<(ComponentAccess, Self::State), InitError> {
        Ok((ComponentAccess::new_true(), ()))
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
        Ok((ComponentAccess::new_true(), ()))
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

/// Transparent wrapper around a [`NonNull`]. This implements [`Send`] and
/// [`Sync`] unconditionally.
#[doc(hidden)]
#[repr(transparent)]
pub struct ColumnPtr<T>(pub NonNull<T>);

impl<T> Clone for ColumnPtr<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> Copy for ColumnPtr<T> {}

impl<T> fmt::Debug for ColumnPtr<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("ColumnPtr").field(&self.0).finish()
    }
}

// SAFETY: `ColumnPtr` is just a wrapper around a pointer, so these impls are
// safe on their own.
unsafe impl<T> Send for ColumnPtr<T> {}
unsafe impl<T> Sync for ColumnPtr<T> {}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::prelude::*;

    #[derive(Event)]
    struct E;

    /// Test for query access conflicts.
    macro_rules! check_access {
        ($name:ident, $succeed:expr, $Q:ty) => {
            #[test]
            fn $name() {
                let mut world = World::new();
                let res = world.try_add_handler(|_: Receiver<E>, _: Fetcher<$Q>| {});
                assert_eq!(res.is_ok(), $succeed, "{res:?}");
            }
        };
    }

    #[derive(Component)]
    struct A;

    #[derive(Component)]
    struct B;

    #[derive(Component)]
    struct C;

    check_access!(t00, true, &mut A);
    check_access!(t01, true, (&mut A, &mut B));
    check_access!(t02, false, (&mut A, &mut A));
    check_access!(t03, false, (&mut A, (&mut A,)));
    check_access!(t04, false, (&mut A, &A));
    check_access!(t05, true, (&A, &A));
    check_access!(t06, true, ());
    check_access!(t07, false, (Option<&A>, &mut A));
    check_access!(t08, false, (&mut A, Option<&B>, &A, Not<&B>));
    check_access!(t09, true, (&mut A, Not<&A>, &A));
    check_access!(t10, false, Or<&mut A, &mut A>);
    check_access!(t11, true, Xor<&mut A, &mut A>);
    check_access!(t12, false, Or<(&A, &B), (&mut B, &C)>);
    check_access!(t13, true, (Xor<&mut A, &mut A>, &A));
    check_access!(t14, true, (Option<&A>, &A, &A));
    check_access!(t15, false, (Xor<(&A, &B), (&B, &C)>, &mut B));
    check_access!(t16, true, (Xor<(&A, &B), (&B, &C)>, &B));
    check_access!(
        t17,
        true,
        Or<Or<(&mut A, With<&B>), (&A, Not<&B>)>, (&A, Not<&B>)>
    );
    check_access!(
        t18,
        true,
        (((&mut A, With<&B>), (&A, Not<&B>)), (&A, Not<&B>))
    );
    check_access!(t19, true, (&mut A, &A, Not<&A>));
    check_access!(t20, true, (Not<&A>, &mut A, &A));

    macro_rules! check_matching {
        (
            name = $name:ident,
            query = $query:ty,
            matches = [$(($($matched_comp:ident),*)),*],
            ignores = [$(($($ignored_comp:ident),*)),*],
        ) => {
            #[test]
            fn $name() {
                let mut world = World::new();
                #[allow(unused_mut)]
                let mut matching = crate::map::IndexSet::<EntityId>::default();

                $(
                    let e = world.spawn();
                    matching.insert(e);
                    $(
                        world.insert(e, $matched_comp);
                    )*
                )*

                $(
                    #[allow(unused_variables)]
                    let e = world.spawn();
                    $(
                        world.insert(e, $ignored_comp);
                    )*
                )*

                #[derive(Component)]
                struct Matching(crate::map::IndexSet<EntityId>);

                let matching_entity = world.spawn();
                world.insert(matching_entity, Matching(matching));

                world.add_handler(move |_: Receiver<E>, f: Fetcher<(EntityId, $query)>, matching: Single<&mut Matching>| {
                    for (e, _) in f {
                        if e != matching_entity && !matching.0.0.shift_remove(&e) {
                            panic!("matched entity unexpectedly {e:?}");
                        }
                    }
                });

                world.send(E);

                let matching = &world.get::<Matching>(matching_entity).unwrap().0;

                if !matching.is_empty() {
                    panic!("entities not matched: {matching:?}");
                }
            }
        }
    }

    check_matching! {
        name = matching_ref,
        query = &A,
        matches = [(A), (A, B), (A, B, C)],
        ignores = [(), (B), (C), (B, C)],
    }

    check_matching! {
        name = matching_with,
        query = With<&A>,
        matches = [(A), (A, B), (A, B, C)],
        ignores = [(), (B), (C), (B, C)],
    }

    check_matching! {
        name = matching_and,
        query = (&A, &mut B),
        matches = [(A, B), (A, B, C)],
        ignores = [(), (A), (B), (A, C), (B, C)],
    }

    check_matching! {
        name = matching_or,
        query = Or<&A, &B>,
        matches = [(A), (B), (A, B), (A, B, C), (B, C)],
        ignores = [(), (C)],
    }

    check_matching! {
        name = matching_xor,
        query = Xor<&A, &mut B>,
        matches = [(A), (B), (A, C), (B, C)],
        ignores = [(), (A, B), (A, B, C)],
    }

    check_matching! {
        name = matching_option,
        query = Option<&A>,
        matches = [(), (A), (A, B), (A, B, C)],
        ignores = [],
    }

    check_matching! {
        name = matching_entity_id,
        query = EntityId,
        matches = [(), (A), (A, B), (A, B, C)],
        ignores = [],
    }

    check_matching! {
        name = matching_not,
        query = Not<&A>,
        matches = [(), (B), (B, C)],
        ignores = [(A), (A, B), (A, B, C)],
    }

    check_matching! {
        name = matching_and_not,
        query = (&mut A, Not<&B>),
        matches = [(A), (A, C)],
        ignores = [(), (A, B), (B, C), (A, B, C)],
    }

    check_matching! {
        name = matching_phantom_data,
        query = PhantomData<()>,
        matches = [(), (A), (A, B), (A, B, C)],
        ignores = [],
    }

    #[test]
    #[allow(dead_code)]
    fn derived_query() {
        #[derive(Query)]
        struct UnitQuery;

        #[derive(Query)]
        struct QueryWithLifetime<'a> {
            foo: &'a A,
        }

        #[derive(Query)]
        struct QueryWithTwoLifetimes<'a, 'b> {
            foo: &'a A,
            bar: &'b B,
        }

        #[derive(Query)]
        struct QueryWithTypeParam<'a, T> {
            foo: &'a A,
            bar: T,
        }

        #[derive(Query)]
        struct TupleStructQuery<'a>(&'a A, &'a mut B);

        assert_read_only_query::<UnitQuery>();
        assert_read_only_query::<QueryWithLifetime>();
        assert_read_only_query::<QueryWithTwoLifetimes>();
        assert_read_only_query::<QueryWithTypeParam<()>>();

        fn assert_read_only_query<Q: ReadOnlyQuery>() {}
    }
}
