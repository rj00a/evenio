use core::{any::{self, TypeId}, fmt};

use alloc::borrow::Cow;
use evenio_macros::all_tuples;

use super::{Handler, HandlerConfig, HandlerInfo, HandlerParam, InitError};
use crate::{archetype::Archetype, entity::EntityLocation, event::EventPtr, world::{UnsafeWorldCell, World}};

/// The [`Handler`] implementation for ordinary functions.
///
/// This is obtained by using the [`IntoHandler`] impl on functions which accept
/// only [`HandlerParam`]s.
pub struct FunctionHandler<Marker, F: HandlerParamFunction<Marker>> {
    func: F,
    state: Option<<F::Param as HandlerParam>::State>,
}

impl<Marker, F> FunctionHandler<Marker, F>
where
    F: HandlerParamFunction<Marker>,
{
    /// Create a new uninitialized function handler.
    pub fn new(func: F) -> Self {
        Self { func, state: None }
    }
}

impl<Marker, F> fmt::Debug for FunctionHandler<Marker, F>
where
    F: HandlerParamFunction<Marker> + fmt::Debug,
    <F::Param as HandlerParam>::State: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("FunctionHandler")
            .field("func", &self.func)
            .field("state", &self.state)
            .finish()
    }
}

impl<Marker, F> Handler for FunctionHandler<Marker, F>
where
    F: HandlerParamFunction<Marker>,
    Marker: 'static,
{
    fn type_id(&self) -> Option<TypeId> {
        Some(TypeId::of::<F>())
    }

    fn name(&self) -> Cow<'static, str> {
        Cow::Borrowed(any::type_name::<F>())
    }

    fn init(&mut self, world: &mut World, config: &mut HandlerConfig) -> Result<(), InitError> {
        self.state = Some(<F::Param as HandlerParam>::init(world, config)?);
        Ok(())
    }

    unsafe fn run(
        &mut self,
        handler_info: &HandlerInfo,
        event_ptr: EventPtr,
        target_location: EntityLocation,
        world: UnsafeWorldCell,
    ) {
        let state = unsafe {
            self.state
                .as_mut()
                .expect_debug_checked("handler must be initialized")
        };

        let param =
            <F::Param as HandlerParam>::get(state, handler_info, event_ptr, target_location, world);
        self.func.run(param);
    }

    fn refresh_archetype(&mut self, arch: &Archetype) {
        let state = unsafe {
            self.state
                .as_mut()
                .expect_debug_checked("handler must be initialized")
        };

        F::Param::refresh_archetype(state, arch)
    }

    fn remove_archetype(&mut self, arch: &Archetype) {
        let state = unsafe {
            self.state
                .as_mut()
                .expect_debug_checked("handler must be initialized")
        };

        F::Param::remove_archetype(state, arch)
    }
}

/// Trait for functions whose parameters are [`HandlerParam`]s.
pub trait HandlerParamFunction<Marker>: Send + Sync + 'static {
    /// The handler params used by this function, combined into a single type.
    type Param: HandlerParam;

    /// Call the function.
    fn run(&mut self, param: <Self::Param as HandlerParam>::Item<'_>);
}

macro_rules! impl_handler_param_function {
    ($(($P:ident, $p:ident)),*) => {
        impl<F, $($P: HandlerParam),*> HandlerParamFunction<fn($($P),*)> for F
        where
            F: FnMut($($P),*) + FnMut($($P::Item<'_>),*) + Send + Sync + 'static,
        {
            type Param = ($($P,)*);

            fn run(
                &mut self,
                ($($p,)*): <Self::Param as HandlerParam>::Item<'_>
            ) {
                (self)($($p),*)
            }
        }
    }
}

all_tuples!(impl_handler_param_function, 0, 15, P, p);

#[doc(hidden)]
#[derive(Debug)]
pub struct FunctionHandlerMarker;

impl<Marker, F> IntoHandler<(FunctionHandlerMarker, Marker)> for F
where
    Marker: 'static,
    F: HandlerParamFunction<Marker>,
{
    type Handler = FunctionHandler<Marker, F>;

    fn into_handler(self) -> Self::Handler {
        FunctionHandler::new(self)
    }
}

impl<H: Handler> IntoHandler<()> for H {
    type Handler = Self;

    fn into_handler(self) -> Self::Handler {
        self
    }
}