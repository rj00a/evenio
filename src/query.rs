use core::fmt;
use std::any;
use std::marker::PhantomData;
use std::ptr::NonNull;

use evenio_macros::all_tuples;

use crate::access::{Access, ComponentAccessExpr};
use crate::archetype::{Archetype, ArchetypeRow};
use crate::component::{Component, ComponentId};
use crate::entity::EntityId;
use crate::system::{Config, InitError};
use crate::world::World;
pub unsafe trait Query {
    /// The item returned by this query. This is usually same type as `Self`,
    /// but with a transformed lifetime.
    type Item<'a>;
    /// Per-archetype state.
    type Fetch: Send + Sync + fmt::Debug + 'static;
    /// Cached data for fetch initialization. This is stored in [`QueryState`].
    type State: Send + Sync + fmt::Debug + 'static;

    fn init(
        world: &mut World,
        config: &mut Config,
    ) -> Result<(ComponentAccessExpr, Self::State), InitError>;

    fn init_fetch(archetype: &Archetype, state: &mut Self::State) -> Option<Self::Fetch>;

    unsafe fn fetch_mut<'a>(fetch: &mut Self::Fetch, row: ArchetypeRow) -> Self::Item<'a>;
}

pub trait ReadOnlyQuery: Query {
    unsafe fn fetch<'a>(fetch: &Self::Fetch, row: ArchetypeRow) -> Self::Item<'a>;
}

unsafe impl<C: Component> Query for &'_ C {
    type Item<'a> = &'a C;

    type Fetch = ColumnPtr<C>;

    type State = ComponentId;

    fn init(
        world: &mut World,
        config: &mut Config,
    ) -> Result<(ComponentAccessExpr, Self::State), InitError> {
        let id = world.add_component::<C>();
        let expr = ComponentAccessExpr::with(id.index(), Access::Read);

        Ok((expr, id))
    }

    fn init_fetch(archetype: &Archetype, state: &mut Self::State) -> Option<Self::Fetch> {
        archetype
            .column_of(state.index())
            .map(|c| ColumnPtr(c.data().cast()))
    }

    unsafe fn fetch_mut<'a>(fetch: &mut Self::Fetch, row: ArchetypeRow) -> Self::Item<'a> {
        Self::fetch(fetch, row)
    }
}

impl<C: Component> ReadOnlyQuery for &'_ C {
    unsafe fn fetch<'a>(fetch: &Self::Fetch, row: ArchetypeRow) -> Self::Item<'a> {
        &*fetch.0.as_ptr().cast_const().add(row.0 as usize)
    }
}

unsafe impl<C: Component> Query for &'_ mut C {
    type Item<'a> = &'a mut C;

    type Fetch = ColumnPtr<C>;

    type State = ComponentId;

    fn init(
        world: &mut World,
        config: &mut Config,
    ) -> Result<(ComponentAccessExpr, Self::State), InitError> {
        struct AssertMutable<C>(PhantomData<C>);

        impl<C: Component> AssertMutable<C> {
            const ASSERTION: () = assert!(
                C::MUTABLE,
                "component does not permit mutation through mutable references (see \
                 `Component::MUTABLE`)."
            );
        }

        let _ = AssertMutable::<C>::ASSERTION;

        let id = world.add_component::<C>();
        let expr = ComponentAccessExpr::with(id.index(), Access::ReadWrite);

        Ok((expr, id))
    }

    fn init_fetch(archetype: &Archetype, state: &mut Self::State) -> Option<Self::Fetch> {
        <&C>::init_fetch(archetype, state)
    }

    unsafe fn fetch_mut<'a>(fetch: &mut Self::Fetch, row: ArchetypeRow) -> Self::Item<'a> {
        &mut *fetch.0.as_ptr().add(row.0 as usize)
    }
}

macro_rules! impl_query_tuple {
    ($(($Q:ident, $q:ident)),*) => {
        unsafe impl<$($Q: Query),*> Query for ($($Q,)*) {
            type Item<'a> = ($($Q::Item<'a>,)*);

            type Fetch = ($($Q::Fetch,)*);

            type State = ($($Q::State,)*);

            fn init(world: &mut World, config: &mut Config) -> Result<(ComponentAccessExpr, Self::State), InitError> {
                #![allow(unused_variables)]

                #[allow(unused_mut)]
                let mut res = ComponentAccessExpr::new();

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

            fn init_fetch(archetype: &Archetype, ($($q,)*): &mut Self::State) -> Option<Self::Fetch> {
                Some((
                    $(
                        $Q::init_fetch(archetype, $q)?,
                    )*
                ))
            }

            unsafe fn fetch_mut<'a>(($($q,)*): &mut Self::Fetch, row: ArchetypeRow) -> Self::Item<'a> {
                (
                    $(
                        $Q::fetch_mut($q, row),
                    )*
                )
            }
        }

        impl<$($Q: ReadOnlyQuery),*> ReadOnlyQuery for ($($Q,)*) {
            unsafe fn fetch<'a>(($($q,)*): &Self::Fetch, row: ArchetypeRow) -> Self::Item<'a> {
                (
                    $(
                        $Q::fetch($q, row),
                    )*
                )
            }
        }
    }
}

// Debug impls for tuples only go up to arity 12.
all_tuples!(impl_query_tuple, 0, 12, Q, q);

unsafe impl<Q: Query> Query for Option<Q> {
    type Item<'a> = Option<Q::Item<'a>>;

    type Fetch = Option<Q::Fetch>;

    type State = Q::State;

    fn init(
        world: &mut World,
        config: &mut Config,
    ) -> Result<(ComponentAccessExpr, Self::State), InitError> {
        let (mut expr, state) = Q::init(world, config)?;

        expr = expr.or(&ComponentAccessExpr::new()).unwrap();

        Ok((expr, state))
    }

    fn init_fetch(archetype: &Archetype, state: &mut Self::State) -> Option<Self::Fetch> {
        Some(Q::init_fetch(archetype, state))
    }

    unsafe fn fetch_mut<'a>(fetch: &mut Self::Fetch, row: ArchetypeRow) -> Self::Item<'a> {
        fetch.as_mut().map(|f| Q::fetch_mut(f, row))
    }
}

impl<Q: ReadOnlyQuery> ReadOnlyQuery for Option<Q> {
    unsafe fn fetch<'a>(fetch: &Self::Fetch, row: ArchetypeRow) -> Self::Item<'a> {
        fetch.as_ref().map(|f| Q::fetch(f, row))
    }
}

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

    type Fetch = Or<L::Fetch, R::Fetch>;

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

    fn init_fetch(
        archetype: &Archetype,
        (left_state, right_state): &mut Self::State,
    ) -> Option<Self::Fetch> {
        match (
            L::init_fetch(archetype, left_state),
            R::init_fetch(archetype, right_state),
        ) {
            (None, None) => None,
            (None, Some(r)) => Some(Or::Right(r)),
            (Some(l), None) => Some(Or::Left(l)),
            (Some(l), Some(r)) => Some(Or::Both(l, r)),
        }
    }

    unsafe fn fetch_mut<'a>(fetch: &mut Self::Fetch, row: ArchetypeRow) -> Self::Item<'a> {
        fetch
            .as_mut()
            .map(|l| L::fetch_mut(l, row), |r| R::fetch_mut(r, row))
    }
}

impl<L, R> ReadOnlyQuery for Or<L, R>
where
    L: ReadOnlyQuery,
    R: ReadOnlyQuery,
{
    unsafe fn fetch<'a>(fetch: &Self::Fetch, row: ArchetypeRow) -> Self::Item<'a> {
        fetch
            .as_ref()
            .map(|l| L::fetch(l, row), |r| R::fetch(r, row))
    }
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

    type Fetch = Xor<L::Fetch, R::Fetch>;

    type State = (L::State, R::State);

    fn init(
        world: &mut World,
        config: &mut Config,
    ) -> Result<(ComponentAccessExpr, Self::State), InitError> {
        let (mut left_expr, left_state) = L::init(world, config)?;
        let (right_expr, right_state) = R::init(world, config)?;

        Ok((left_expr.xor(&right_expr), (left_state, right_state)))
    }

    fn init_fetch(
        archetype: &Archetype,
        (left_state, right_state): &mut Self::State,
    ) -> Option<Self::Fetch> {
        match (
            L::init_fetch(archetype, left_state),
            R::init_fetch(archetype, right_state),
        ) {
            (None, None) => None,
            (None, Some(r)) => Some(Xor::Right(r)),
            (Some(l), None) => Some(Xor::Left(l)),
            (Some(_), Some(_)) => None,
        }
    }

    unsafe fn fetch_mut<'a>(fetch: &mut Self::Fetch, row: ArchetypeRow) -> Self::Item<'a> {
        fetch
            .as_mut()
            .map(|l| L::fetch_mut(l, row), |r| R::fetch_mut(r, row))
    }
}

impl<L, R> ReadOnlyQuery for Xor<L, R>
where
    L: ReadOnlyQuery,
    R: ReadOnlyQuery,
{
    unsafe fn fetch<'a>(fetch: &Self::Fetch, row: ArchetypeRow) -> Self::Item<'a> {
        fetch
            .as_ref()
            .map(|l| L::fetch(l, row), |r| R::fetch(r, row))
    }
}

pub struct Not<Q>(PhantomData<fn(Q)>);

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

    type Fetch = ();

    type State = Q::State;

    fn init(
        world: &mut World,
        config: &mut Config,
    ) -> Result<(ComponentAccessExpr, Self::State), InitError> {
        let (expr, state) = Q::init(world, config)?;

        Ok((expr.not(), state))
    }

    fn init_fetch(archetype: &Archetype, state: &mut Self::State) -> Option<Self::Fetch> {
        match Q::init_fetch(archetype, state) {
            Some(_) => None,
            None => Some(()),
        }
    }

    unsafe fn fetch_mut<'a>(_fetch: &mut Self::Fetch, _row: ArchetypeRow) -> Self::Item<'a> {
        Not::new()
    }
}

impl<Q: Query> ReadOnlyQuery for Not<Q> {
    unsafe fn fetch<'a>(_fetch: &Self::Fetch, _row: ArchetypeRow) -> Self::Item<'a> {
        Not::new()
    }
}

pub struct With<Q>(PhantomData<fn(Q)>);

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

    type Fetch = ();

    type State = Q::State;

    fn init(
        world: &mut World,
        config: &mut Config,
    ) -> Result<(ComponentAccessExpr, Self::State), InitError> {
        let (mut expr, state) = Q::init(world, config)?;

        expr.access.clear();

        Ok((expr, state))
    }

    fn init_fetch(archetype: &Archetype, state: &mut Self::State) -> Option<Self::Fetch> {
        Q::init_fetch(archetype, state).map(|_| ())
    }

    unsafe fn fetch_mut<'a>(_fetch: &mut Self::Fetch, _row: ArchetypeRow) -> Self::Item<'a> {
        With::new()
    }
}

impl<Q: Query> ReadOnlyQuery for With<Q> {
    unsafe fn fetch<'a>(_fetch: &Self::Fetch, _row: ArchetypeRow) -> Self::Item<'a> {
        With::new()
    }
}

pub struct Has<Q> {
    has: bool,
    _marker: PhantomData<fn(Q)>,
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

    type Fetch = bool;

    type State = Q::State;

    fn init(
        world: &mut World,
        config: &mut Config,
    ) -> Result<(ComponentAccessExpr, Self::State), InitError> {
        let (_, state) = Q::init(world, config)?;

        Ok((ComponentAccessExpr::new(), state))
    }

    fn init_fetch(archetype: &Archetype, state: &mut Self::State) -> Option<Self::Fetch> {
        Some(Q::init_fetch(archetype, state).is_some())
    }

    unsafe fn fetch_mut<'a>(fetch: &mut Self::Fetch, row: ArchetypeRow) -> Self::Item<'a> {
        Self::fetch(fetch, row)
    }
}

impl<Q: Query> ReadOnlyQuery for Has<Q> {
    unsafe fn fetch<'a>(fetch: &Self::Fetch, _row: ArchetypeRow) -> Self::Item<'a> {
        Has::new(*fetch)
    }
}

unsafe impl Query for EntityId {
    type Item<'a> = Self;

    type Fetch = ColumnPtr<EntityId>;

    type State = ();

    fn init(
        world: &mut World,
        config: &mut Config,
    ) -> Result<(ComponentAccessExpr, Self::State), InitError> {
        Ok((ComponentAccessExpr::new(), ()))
    }

    fn init_fetch(archetype: &Archetype, (): &mut Self::State) -> Option<Self::Fetch> {
        todo!()
        /*
        Some(ColumnPtr(archetype.entity_ids()))
        */
    }

    unsafe fn fetch_mut<'a>(fetch: &mut Self::Fetch, row: ArchetypeRow) -> Self::Item<'a> {
        *fetch.0.as_ptr().add(row.0 as usize)
    }
}

impl ReadOnlyQuery for EntityId {
    unsafe fn fetch<'a>(fetch: &Self::Fetch, row: ArchetypeRow) -> Self::Item<'a> {
        *fetch.0.as_ptr().cast_const().add(row.0 as usize)
    }
}

pub struct ColumnPtr<T>(NonNull<T>);

impl<T> Clone for ColumnPtr<T> {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

impl<T> Copy for ColumnPtr<T> {}

impl<T> fmt::Debug for ColumnPtr<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("Column").field(&self.0).finish()
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
        ($name:ident, $succeed:expr, $q:ty) => {
            #[test]
            fn $name() {
                assert_eq!(check_query::<$q>(), $succeed);
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
