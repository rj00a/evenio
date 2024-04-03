//! Type-level DSL for retrieving data from entities.

mod xor;
mod has;
mod not;
mod option;
mod tuple;
mod or;
mod with;
mod misc;

pub use xor::*;
pub use has::*;
pub use not::*;
pub use or::*;
pub use with::*;

use core::fmt;
use core::marker::PhantomData;
use core::ptr::NonNull;

use evenio_macros::all_tuples;
pub use evenio_macros::Query;

use crate::access::{Access, ArchetypeFilter, ComponentAccess};
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
    ///   the [`ComponentAccessExpr`] returned by [`init`].
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

    fn check_query<Q: Query + 'static>() -> bool {
        let r = std::panic::catch_unwind(|| {
            let mut world = World::new();

            (world.add_handler(|_: Receiver<E>, _: Fetcher<Q>| {}), world)
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
    t!(
        t17,
        true,
        Or<Or<(&mut A, With<&B>), (&A, Not<&B>)>, (&A, Not<&B>)>
    );
    t!(
        t18,
        true,
        (((&mut A, With<&B>), (&A, Not<&B>)), (&A, Not<&B>))
    );
    t!(t19, true, (&mut A, &A, Not<&A>));
    t!(t20, true, (Not<&A>, &mut A, &A));

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
