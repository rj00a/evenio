use core::any::TypeId;
use core::fmt;
use std::any;
use std::marker::PhantomData;
use std::ptr::NonNull;

use evenio_macros::all_tuples;
pub use evenio_macros::Query;

use crate::access::{Access, AccessMap, ComponentAccessExpr};
use crate::archetype::{Archetype, ArchetypeRow};
use crate::component::{Component, ComponentId, ComponentIdx, AssertMutable};
use crate::debug_checked::UnwrapDebugChecked;
use crate::entity::EntityId;
use crate::system::{Config, InitError};
use crate::world::{UnsafeWorldCell, World};

/// # Deriving
///
/// ```
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
/// world.add_system(|_: Receiver<MyEvent>, q: Fetcher<CustomQuery>| {
///     for item in q {
///         println!("{item:?}");
///     }
/// });
///
/// world.send(MyEvent);
/// # #[derive(Event)]
/// # struct MyEvent;
/// ```
pub unsafe trait Query {
    /// The item returned by this query. This is usually the same type as
    /// `Self`, but with a modified lifetime.
    type Item<'a>;
    /// Per-archetype state.
    type ArchState: Send + Sync + fmt::Debug + 'static;
    /// Cached data for fetch initialization. This is stored in
    /// [`FetcherState`].
    type State: Send + Sync + fmt::Debug + 'static;

    fn init(
        world: &mut World,
        config: &mut Config,
    ) -> Result<(ComponentAccessExpr, Self::State), InitError>;

    fn new_state(world: &mut World) -> Self::State;

    fn new_arch_state(arch: &Archetype, state: &mut Self::State) -> Option<Self::ArchState>;

    unsafe fn get<'a>(state: &Self::ArchState, row: ArchetypeRow) -> Self::Item<'a>;
}

pub unsafe trait ReadOnlyQuery: Query {}

unsafe impl<C: Component> Query for &'_ C {
    type Item<'a> = &'a C;

    type ArchState = ColumnPtr<C>;

    type State = ComponentId;

    fn init(
        world: &mut World,
        _config: &mut Config,
    ) -> Result<(ComponentAccessExpr, Self::State), InitError> {
        let id = Self::new_state(world);
        let expr = ComponentAccessExpr::with(id.index(), Access::Read);

        Ok((expr, id))
    }

    fn new_state(world: &mut World) -> Self::State {
        world.add_component::<C>()
    }

    fn new_arch_state(arch: &Archetype, state: &mut Self::State) -> Option<Self::ArchState> {
        arch.column_of(state.index())
            .map(|c| ColumnPtr(c.data().cast()))
    }

    unsafe fn get<'a>(state: &Self::ArchState, row: ArchetypeRow) -> Self::Item<'a> {
        &*state.0.as_ptr().cast_const().add(row.0 as usize)
    }
}

unsafe impl<C: Component> ReadOnlyQuery for &'_ C {}

unsafe impl<C: Component> Query for &'_ mut C {
    type Item<'a> = &'a mut C;

    type ArchState = ColumnPtr<C>;

    type State = ComponentId;

    fn init(
        world: &mut World,
        _config: &mut Config,
    ) -> Result<(ComponentAccessExpr, Self::State), InitError> {
        let _ = AssertMutable::<C>::ASSERTION;

        let id = Self::new_state(world);
        let expr = ComponentAccessExpr::with(id.index(), Access::ReadWrite);

        Ok((expr, id))
    }

    fn new_state(world: &mut World) -> Self::State {
        world.add_component::<C>()
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
        #[allow(unused_variables)]
        unsafe impl<$($Q: Query),*> Query for ($($Q,)*) {
            type Item<'a> = ($($Q::Item<'a>,)*);

            type ArchState = ($($Q::ArchState,)*);

            type State = ($($Q::State,)*);

            fn init(
                world: &mut World,
                config: &mut Config
            ) -> Result<(ComponentAccessExpr, Self::State), InitError> {
                #![allow(unused_variables)]

                #[allow(unused_mut)]
                let mut res = ComponentAccessExpr::new(true);

                $(
                    let (expr, $q) = $Q::init(world, config)?;

                    let Ok(expr) = res.and(&expr) else {
                        return Err(InitError(format!(
                            "conflicting access in tuple `{}`: tuple element `{}` conflicts with previous elements",
                            any::type_name::<Self>(),
                            any::type_name::<$Q>(),
                        ).into()));
                    };

                    res = expr;
                )*

                Ok((res, ($($q,)*)))
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

unsafe impl<Q: Query> Query for Option<Q> {
    type Item<'a> = Option<Q::Item<'a>>;

    type ArchState = Option<Q::ArchState>;

    type State = Q::State;

    fn init(
        world: &mut World,
        config: &mut Config,
    ) -> Result<(ComponentAccessExpr, Self::State), InitError> {
        let (mut expr, state) = Q::init(world, config)?;

        expr = expr.or(&ComponentAccessExpr::new(true)).unwrap();

        Ok((expr, state))
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

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum Or<L, R> {
    Left(L),
    Right(R),
    Both(L, R),
}

impl<L, R> Or<L, R> {
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

    pub const fn as_ref(&self) -> Or<&L, &R> {
        match self {
            Or::Left(l) => Or::Left(l),
            Or::Right(r) => Or::Right(r),
            Or::Both(l, r) => Or::Both(l, r),
        }
    }

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
    ) -> Result<(ComponentAccessExpr, Self::State), InitError> {
        let (left_expr, left_state) = L::init(world, config)?;
        let (right_expr, right_state) = R::init(world, config)?;

        let Ok(expr) = left_expr.or(&right_expr) else {
            return Err(InitError(
                format!(
                    "conflicting query in `{}` (both operands of an OR query may be active at the \
                     same time)",
                    any::type_name::<Self>()
                )
                .into(),
            ));
        };

        Ok((expr, (left_state, right_state)))
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

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum Xor<L, R> {
    Left(L),
    Right(R),
}

impl<L, R> Xor<L, R> {
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

    pub const fn as_ref(&self) -> Xor<&L, &R> {
        match self {
            Xor::Left(l) => Xor::Left(l),
            Xor::Right(r) => Xor::Right(r),
        }
    }

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
        config: &mut Config,
    ) -> Result<(ComponentAccessExpr, Self::State), InitError> {
        let (left_expr, left_state) = L::init(world, config)?;
        let (right_expr, right_state) = R::init(world, config)?;

        Ok((left_expr.xor(&right_expr), (left_state, right_state)))
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

pub struct Not<Q>(PhantomData<fn() -> Q>);

impl<Q> Not<Q> {
    pub const fn new() -> Self {
        Self(PhantomData)
    }
}

impl<Q> Clone for Not<Q> {
    fn clone(&self) -> Self {
        Self::new()
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
        config: &mut Config,
    ) -> Result<(ComponentAccessExpr, Self::State), InitError> {
        let (expr, state) = Q::init(world, config)?;

        Ok((expr.not(), state))
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

pub struct With<Q>(PhantomData<fn() -> Q>);

impl<Q> With<Q> {
    pub const fn new() -> Self {
        Self(PhantomData)
    }
}

impl<Q> Clone for With<Q> {
    fn clone(&self) -> Self {
        Self(self.0.clone())
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
        config: &mut Config,
    ) -> Result<(ComponentAccessExpr, Self::State), InitError> {
        let (mut expr, state) = Q::init(world, config)?;

        expr.access.clear();

        Ok((expr, state))
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

pub struct Has<Q> {
    has: bool,
    _marker: PhantomData<fn() -> Q>,
}

impl<Q> Has<Q> {
    pub const fn new(has: bool) -> Self {
        Self {
            has,
            _marker: PhantomData,
        }
    }

    pub const fn get(self) -> bool {
        self.has
    }
}

impl<Q> Clone for Has<Q> {
    fn clone(&self) -> Self {
        Self {
            has: self.has,
            _marker: PhantomData,
        }
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
        config: &mut Config,
    ) -> Result<(ComponentAccessExpr, Self::State), InitError> {
        let (_, state) = Q::init(world, config)?;

        Ok((ComponentAccessExpr::new(true), state))
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

unsafe impl Query for EntityId {
    type Item<'a> = Self;

    type ArchState = ColumnPtr<EntityId>;

    type State = ();

    fn init(
        _world: &mut World,
        _config: &mut Config,
    ) -> Result<(ComponentAccessExpr, Self::State), InitError> {
        Ok((ComponentAccessExpr::new(true), ()))
    }

    fn new_state(_world: &mut World) -> Self::State {
        ()
    }

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
        _config: &mut Config,
    ) -> Result<(ComponentAccessExpr, Self::State), InitError> {
        Ok((ComponentAccessExpr::new(true), ()))
    }

    fn new_state(_world: &mut World) -> Self::State {
        ()
    }

    fn new_arch_state(_arch: &Archetype, _state: &mut Self::State) -> Option<Self::ArchState> {
        Some(())
    }

    unsafe fn get<'a>(_state: &Self::ArchState, _row: ArchetypeRow) -> Self::Item<'a> {
        Self
    }
}

unsafe impl<T: ?Sized> ReadOnlyQuery for PhantomData<T> {}

pub struct ColumnPtr<T>(NonNull<T>);

impl<T> Clone for ColumnPtr<T> {
    fn clone(&self) -> Self {
        Self(self.0.clone())
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

    fn check_query<Q: Query + 'static>() -> bool {
        let r = std::panic::catch_unwind(|| {
            let mut world = World::new();

            (world.add_system(|_: Receiver<E>, _: Fetcher<Q>| {}), world)
        });

        if let Ok((_, mut world)) = r {
            world.send(E);
            true
        } else {
            false
        }
    }

    /// Test for query access conflicts.
    macro_rules! t {
        ($name:ident, $succeed:expr, $Q:ty) => {
            #[test]
            fn $name() {
                assert_eq!(check_query::<$Q>(), $succeed);
            }
        };
    }

    #[derive(Component)]
    struct A;

    #[derive(Component)]
    struct B;

    #[derive(Component)]
    struct C;

    t!(t00, true, &mut A);
    t!(t01, true, (&mut A, &mut B));
    t!(t02, false, (&mut A, &mut A));
    t!(t03, false, (&mut A, (&mut A,)));
    t!(t04, false, (&mut A, &A));
    t!(t05, true, (&A, &A));
    t!(t06, true, ());
    t!(t07, false, (Option<&A>, &mut A));
    t!(t08, false, (&mut A, Option<&B>, &A, Not<&B>));
    t!(t09, true, (&mut A, Not<&A>, &A));
    t!(t10, false, Or<&mut A, &mut A>);
    t!(t11, true, Xor<&mut A, &mut A>);
    t!(t12, false, Or<(&A, &B), (&mut B, &C)>);
    t!(t13, true, (Xor<&mut A, &mut A>, &A));
    t!(t14, true, (Option<&A>, &A, &A));
    t!(t15, false, (Xor<(&A, &B), (&B, &C)>, &mut B));
    t!(t16, true, (Xor<(&A, &B), (&B, &C)>, &B));
}
