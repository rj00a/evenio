use core::{any, fmt};

use super::FetcherState;
use crate::archetype::Archetype;
use crate::entity::EntityLocation;
use crate::event::EventPtr;
use crate::handler::{HandlerConfig, HandlerInfo, HandlerParam, InitError};
use crate::query::Query;
use crate::world::{UnsafeWorldCell, World};

/// A [`HandlerParam`] which fetches a single entity from the world.
///
/// If there isn't exactly one entity that matches the [`Query`], a runtime
/// panic occurs. This is useful for representing global variables or singleton
/// entities.
///
/// # Examples
///
/// ```
/// # #[derive(Event)] struct E;
/// use evenio::prelude::*;
///
/// #[derive(Component)]
/// struct MyComponent(i32);
///
/// let mut world = World::new();
///
/// world.add_handler(
///     |_: Receiver<E>, Single(MyComponent(data)): Single<&MyComponent>| {
///         println!("The data is: {data}");
///     },
/// );
///
/// let e = world.spawn();
/// world.insert(e, MyComponent(123));
///
/// world.send(E);
/// ```
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Default, Debug)]
pub struct Single<'a, Q: Query>(pub Q::Item<'a>);

unsafe impl<Q: Query + 'static> HandlerParam for Single<'_, Q> {
    type State = FetcherState<Q>;

    type Item<'a> = Single<'a, Q>;

    fn init(world: &mut World, config: &mut HandlerConfig) -> Result<Self::State, InitError> {
        FetcherState::init(world, config)
    }

    #[track_caller]
    unsafe fn get<'a>(
        state: &'a mut Self::State,
        info: &'a HandlerInfo,
        event_ptr: EventPtr<'a>,
        target_location: EntityLocation,
        world: UnsafeWorldCell<'a>,
    ) -> Self::Item<'a> {
        match TrySingle::get(state, info, event_ptr, target_location, world) {
            TrySingle(Ok(item)) => Single(item),
            TrySingle(Err(e)) => {
                panic!(
                    "failed to fetch exactly one entity matching the query `{}`: {e}",
                    any::type_name::<Q>()
                )
            }
        }
    }

    fn refresh_archetype(state: &mut Self::State, arch: &Archetype) {
        state.refresh_archetype(arch)
    }

    fn remove_archetype(state: &mut Self::State, arch: &Archetype) {
        state.remove_archetype(arch)
    }
}

/// Like [`Single`], but contains a `Result` instead of panicking on error.
///
/// This is useful if you need to explicitly handle the situation where the
/// query does not match exactly one entity.
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct TrySingle<'a, Q: Query>(pub Result<Q::Item<'a>, SingleError>);

unsafe impl<Q: Query + 'static> HandlerParam for TrySingle<'_, Q> {
    type State = FetcherState<Q>;

    type Item<'a> = TrySingle<'a, Q>;

    fn init(world: &mut World, config: &mut HandlerConfig) -> Result<Self::State, InitError> {
        FetcherState::init(world, config)
    }

    unsafe fn get<'a>(
        state: &'a mut Self::State,
        _info: &'a HandlerInfo,
        _event_ptr: EventPtr<'a>,
        _target_location: EntityLocation,
        world: UnsafeWorldCell<'a>,
    ) -> Self::Item<'a> {
        let mut it = state.iter_mut(world.archetypes());

        let Some(item) = it.next() else {
            return TrySingle(Err(SingleError::QueryDoesNotMatch));
        };

        if it.next().is_some() {
            return TrySingle(Err(SingleError::MoreThanOneMatch));
        }

        TrySingle(Ok(item))
    }

    fn refresh_archetype(state: &mut Self::State, arch: &Archetype) {
        state.refresh_archetype(arch)
    }

    fn remove_archetype(state: &mut Self::State, arch: &Archetype) {
        state.remove_archetype(arch)
    }
}

/// Error raised when fetching exactly one entity matching a query fails.
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum SingleError {
    /// Query does not match any entities
    QueryDoesNotMatch,
    /// More than one entity matched the query.
    MoreThanOneMatch,
}

impl fmt::Display for SingleError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let msg = match self {
            SingleError::QueryDoesNotMatch => "query does not match any entities",
            SingleError::MoreThanOneMatch => "more than one entity matched the query",
        };

        write!(f, "{msg}")
    }
}

#[cfg(feature = "std")]
#[cfg_attr(docsrs, doc(cfg(feature = "std")))]
impl std::error::Error for SingleError {}
