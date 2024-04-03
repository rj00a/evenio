use core::fmt;
use core::marker::PhantomData;
use core::ops::{Deref, DerefMut};

use evenio_macros::all_tuples;

use super::{HandlerConfig, HandlerInfo, InitError};
use crate::archetype::Archetype;
use crate::entity::EntityLocation;
use crate::event::EventPtr;
use crate::exclusive::Exclusive;
use crate::world::{UnsafeWorldCell, World};

/// A parameter in a [`Handler`].
///
/// # Deriving
///
/// This trait can be implemented automatically by using the associated derive
/// macro. The macro works if every field of the struct is also a handler param.
///
/// ```
/// use std::marker::PhantomData;
///
/// use evenio::prelude::*;
///
/// #[derive(Component)]
/// struct C;
///
/// #[derive(Event)]
/// struct E;
///
/// #[derive(HandlerParam)]
/// struct MyHandlerParam<'a> {
///     foo: Fetcher<'a, &'static C>,
///     bar: Sender<'a, E>,
/// }
///
/// let mut world = World::new();
///
/// // Add handler which uses our custom handler param.
/// world.add_handler(|_: Receiver<E>, my_param: MyHandlerParam| {
///     // ...
/// });
/// ```
///
/// # Safety
///
/// Implementors must ensure that [`HandlerParam::init`] correctly registers the
/// data accessed by [`HandlerParam::get`].
pub unsafe trait HandlerParam {
    /// Persistent data stored in the handler.
    type State: Send + Sync + 'static;

    /// The type produced by this handler param. This is expected to be the type
    /// of `Self` but with the lifetime of `'a`.
    type Item<'a>;

    /// Initializes the handler using the input [`World`] and [`Config`].
    ///
    /// If initialization fails, [`InitError`] is returned and the handler is
    /// not considered initialized.
    fn init(world: &mut World, config: &mut HandlerConfig) -> Result<Self::State, InitError>;

    /// Obtains a new instance of the handler parameter.
    ///
    /// # Safety
    ///
    /// - `state` must be up to date and originate from [`HandlerParam::init`].
    /// - `info` must be correct for the handler which this is invoked from.
    /// - `event_ptr`: See [`Handler::run`].
    /// - `target_location`: See [`Handler::run`].
    /// - `world` must have permission to access the data configured in
    ///   [`HandlerParam::init`].
    unsafe fn get<'a>(
        state: &'a mut Self::State,
        info: &'a HandlerInfo,
        event_ptr: EventPtr<'a>,
        target_location: EntityLocation,
        world: UnsafeWorldCell<'a>,
    ) -> Self::Item<'a>;

    /// Refresh an archetype for this handler param. Called whenever
    /// [`Handler::refresh_archetype`] is called.
    fn refresh_archetype(state: &mut Self::State, arch: &Archetype);

    /// Remove the given archetype for this handler param. Called whenever
    /// [`Handler::remove_archetype`] is called.
    fn remove_archetype(state: &mut Self::State, arch: &Archetype);
}

unsafe impl<T> HandlerParam for PhantomData<T> {
    type State = ();

    type Item<'a> = Self;

    fn init(_world: &mut World, _config: &mut HandlerConfig) -> Result<Self::State, InitError> {
        Ok(())
    }

    unsafe fn get<'a>(
        _state: &'a mut Self::State,
        _info: &'a HandlerInfo,
        _event_ptr: EventPtr<'a>,
        _target_location: EntityLocation,
        _world: UnsafeWorldCell<'a>,
    ) -> Self::Item<'a> {
        Self
    }

    fn refresh_archetype(_state: &mut Self::State, _arch: &Archetype) {}

    fn remove_archetype(_state: &mut Self::State, _arch: &Archetype) {}
}

macro_rules! impl_handler_param_tuple {
    ($(($P:ident, $s:ident)),*) => {
        #[allow(unused_variables, clippy::unused_unit)]
        unsafe impl<$($P: HandlerParam),*> HandlerParam for ($($P,)*) {
            type State = ($($P::State,)*);

            type Item<'a> = ($($P::Item<'a>,)*);

            #[inline]
            fn init(world: &mut World, config: &mut HandlerConfig) -> Result<Self::State, InitError> {
                Ok((
                    $(
                        $P::init(world, config)?,
                    )*
                ))
            }

            #[inline]
            unsafe fn get<'a>(
                ($($s,)*): &'a mut Self::State,
                info: &'a HandlerInfo,
                event_ptr: EventPtr<'a>,
                target_location: EntityLocation,
                world: UnsafeWorldCell<'a>,
            ) -> Self::Item<'a> {
                (
                    $(
                        $P::get($s, info, event_ptr, target_location, world),
                    )*
                )
            }

            fn refresh_archetype(
                ($($s,)*): &mut Self::State,
                arch: &Archetype
            )
            {
                $(
                    $P::refresh_archetype($s, arch);
                )*
            }

            fn remove_archetype(
                ($($s,)*): &mut Self::State,
                arch: &Archetype
            )
            {
                $(
                    $P::remove_archetype($s, arch);
                )*
            }
        }
    }
}

all_tuples!(impl_handler_param_tuple, 0, 15, P, s);

#[cfg(feature = "std")]
#[cfg_attr(docsrs, doc(cfg(feature = "std")))]
unsafe impl<P: HandlerParam> HandlerParam for std::sync::Mutex<P> {
    type State = P::State;

    type Item<'a> = std::sync::Mutex<P::Item<'a>>;

    fn init(world: &mut World, config: &mut HandlerConfig) -> Result<Self::State, InitError> {
        P::init(world, config)
    }

    unsafe fn get<'a>(
        state: &'a mut Self::State,
        info: &'a HandlerInfo,
        event_ptr: EventPtr<'a>,
        target_location: EntityLocation,
        world: UnsafeWorldCell<'a>,
    ) -> Self::Item<'a> {
        std::sync::Mutex::new(P::get(state, info, event_ptr, target_location, world))
    }

    fn refresh_archetype(state: &mut Self::State, arch: &Archetype) {
        P::refresh_archetype(state, arch)
    }

    fn remove_archetype(state: &mut Self::State, arch: &Archetype) {
        P::remove_archetype(state, arch)
    }
}

#[cfg(feature = "std")]
#[cfg_attr(docsrs, doc(cfg(feature = "std")))]
unsafe impl<P: HandlerParam> HandlerParam for std::sync::RwLock<P> {
    type State = P::State;

    type Item<'a> = std::sync::RwLock<P::Item<'a>>;

    fn init(world: &mut World, config: &mut HandlerConfig) -> Result<Self::State, InitError> {
        P::init(world, config)
    }

    unsafe fn get<'a>(
        state: &'a mut Self::State,
        info: &'a HandlerInfo,
        event_ptr: EventPtr<'a>,
        target_location: EntityLocation,
        world: UnsafeWorldCell<'a>,
    ) -> Self::Item<'a> {
        std::sync::RwLock::new(P::get(state, info, event_ptr, target_location, world))
    }

    fn refresh_archetype(state: &mut Self::State, arch: &Archetype) {
        P::refresh_archetype(state, arch)
    }

    fn remove_archetype(state: &mut Self::State, arch: &Archetype) {
        P::remove_archetype(state, arch)
    }
}

/// A [`HandlerParam`] for storing handler-local state.
///
/// Any type that implements [`Default`] can be wrapped in a `Local`.
///
/// # Examples
///
/// ```
/// use evenio::handler::Local;
/// use evenio::prelude::*;
///
/// #[derive(Event)]
/// struct E;
///
/// let mut world = World::new();
///
/// fn my_handler(_: Receiver<E>, mut counter: Local<u32>) {
///     println!("counter is {counter}");
///     *counter += 1;
/// }
///
/// let handler = world.add_handler(my_handler);
/// world.send(E);
/// world.send(E);
/// world.send(E);
///
/// // Handler is destroyed and re-added, so the local is reset.
/// println!("refreshing handler...");
/// world.remove_handler(handler);
/// world.add_handler(my_handler);
/// world.send(E);
/// world.send(E);
/// ```
///
/// Output:
///
/// ```txt
/// counter is 0
/// counter is 1
/// counter is 2
/// refreshing handler...
/// counter is 0
/// counter is 1
/// ```
#[derive(Debug)]
pub struct Local<'a, T> {
    state: &'a mut T,
}

unsafe impl<T: Default + Send + 'static> HandlerParam for Local<'_, T> {
    type State = Exclusive<T>;

    type Item<'a> = Local<'a, T>;

    fn init(_world: &mut World, _config: &mut HandlerConfig) -> Result<Self::State, InitError> {
        Ok(Exclusive::new(T::default()))
    }

    unsafe fn get<'a>(
        state: &'a mut Self::State,
        _info: &'a HandlerInfo,
        _event_ptr: EventPtr<'a>,
        _target_location: EntityLocation,
        _world: UnsafeWorldCell<'a>,
    ) -> Self::Item<'a> {
        Local {
            state: state.get_mut(),
        }
    }

    fn refresh_archetype(_state: &mut Self::State, _arch: &Archetype) {}

    fn remove_archetype(_state: &mut Self::State, _arch: &Archetype) {}
}

impl<T> Deref for Local<'_, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.state
    }
}

impl<T> DerefMut for Local<'_, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.state
    }
}

impl<T: fmt::Display> fmt::Display for Local<'_, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.state.fmt(f)
    }
}
