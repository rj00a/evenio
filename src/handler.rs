//! Event handlers

mod config;
mod func;
mod handlers;
mod id;
mod info;
mod list;
mod param;

use core::{any::TypeId, fmt};

use alloc::borrow::Cow;
pub use config::*;
pub use evenio_macros::HandlerParam;
pub use func::*;
pub use handlers::*;
pub use id::*;
pub use info::*;
pub use list::*;
pub use param::*;

use crate::archetype::Archetype;
use crate::entity::EntityLocation;
use crate::event::{Event, EventPtr};
use crate::world::{UnsafeWorldCell, World};

/// An [`Event`] handler function that can be added to a [`World`].
///
/// handlers are added to a world using the [`World::add_handler`] method.
///
/// For more information about handlers, see the [tutorial](crate::tutorial).
pub trait Handler: Send + Sync + 'static {
    /// Returns the [`TypeId`] which uniquely identifies this handler, or `None`
    /// if there is none.
    ///
    /// No two handlers with the same [`TypeId`] will exist in the [`World`] at
    /// the same time.
    fn type_id(&self) -> Option<TypeId>;

    /// Returns the name of this handler for debugging purposes.
    fn name(&self) -> Cow<'static, str>;

    /// Initializes the handler. Returns [`InitError`] on failure.
    fn init(&mut self, world: &mut World, config: &mut HandlerConfig) -> Result<(), InitError>;

    /// Execute the handler by passing in the handler's metadata, a pointer to
    /// the received event of the configured type, the entity location of
    /// the event's target, and an [`UnsafeWorldCell`] with permission to
    /// access the data described in the configuration.
    ///
    /// # Safety
    ///
    /// - handler must be initialized via [`init`].
    /// - `info` must be the correct information for this handler.
    /// - `event_ptr` must point to the correct type of event configured by this
    ///   handler in [`init`].
    /// - `target_location` must be a valid location to an entity matching
    ///   [`Config::targeted_event_expr`], unless the event is not targeted.
    /// - `world` must have permission to access all data configured by this
    ///   handler in [`init`].
    ///
    /// [`init`]: Self::init
    unsafe fn run(
        &mut self,
        info: &HandlerInfo,
        event_ptr: EventPtr,
        target_location: EntityLocation,
        world: UnsafeWorldCell,
    );

    /// Notifies the handler that an archetype it might care about had its
    /// internal state updated.
    ///
    /// This is invoked in the following scenarios:
    /// - The archetype was previously empty, but has now gained at least one
    ///   entity.
    /// - The archetype's columns were reallocated, and any pointers into the
    ///   archetype's columns need to be reacquired.
    ///
    /// This method must not be called with empty archetypes.
    fn refresh_archetype(&mut self, arch: &Archetype);

    /// Notifies the handler that an archetype it might care about is no longer
    /// available.
    ///
    /// This is invoked in the following scenarios:
    /// - The archetype was removed from the world.
    /// - The archetype previously had entities in it, but is now empty.
    ///
    /// In either case, the handler must assume that the archetype is no longer
    /// available. Attempting to read the component data from a removed
    /// archetype is illegal.
    fn remove_archetype(&mut self, arch: &Archetype);
}

/// Types which can be converted into [`Handler`]s.
///
/// This trait is implemented for all functions that return `()` and whose
/// arguments are all [`HandlerParam`]s.
pub trait IntoHandler<Marker>: Sized {
    /// The handler type to convert to.
    type Handler: Handler;

    /// Performs the conversion into a [`Handler`].
    fn into_handler(self) -> Self::Handler;

    /// Ignore this handler's reported [`TypeId`]. This can be used to add a
    /// specific handler to the world more than once.
    ///
    /// # Examples
    ///
    /// ```
    /// use evenio::prelude::*;
    ///
    /// let mut world = World::new();
    ///
    /// let id_1 = world.add_handler(my_handler);
    /// let id_2 = world.add_handler(my_handler.no_type_id());
    /// let id_3 = world.add_handler(my_handler);
    ///
    /// assert_ne!(id_1, id_2);
    /// assert_eq!(id_1, id_3);
    /// #
    /// # fn my_handler(_: Receiver<E>) {}
    /// #
    /// # #[derive(Event)]
    /// # struct E;
    /// ```
    fn no_type_id(self) -> NoTypeId<Self::Handler> {
        NoTypeId(self.into_handler())
    }

    /// Returns a wrapper which sets the priority of this handler to
    /// [`Priority::High`].
    fn high(self) -> High<Self::Handler> {
        High(self.into_handler())
    }

    /// Returns a wrapper which sets the priority of this handler to
    /// [`Priority::Low`].
    fn low(self) -> Low<Self::Handler> {
        Low(self.into_handler())
    }
}

/// An error returned when handler initialization fails. Contains an error
/// message.
///
/// The error message is not stable.
#[derive(Clone, Debug)]
pub struct InitError(pub Box<str>);

impl fmt::Display for InitError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", &self.0)
    }
}

#[cfg(feature = "std")]
#[cfg_attr(docsrs, doc(cfg(feature = "std")))]
impl std::error::Error for InitError {}

/// An event sent immediately after a new handler is added to the world.
/// Contains the ID of the added handler.
#[derive(Event, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct AddHandler(pub HandlerId);

/// An event sent immediately before a handler is removed from the world.
/// Contains the ID of the handler to be removed.
#[derive(Event, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct RemoveHandler(pub HandlerId);

#[cfg(test)]
mod tests {
    use evenio::prelude::*;

    use super::*;
    use crate::event::Events;

    #[test]
    #[allow(dead_code)]
    fn derive_handler_param() {
        #[derive(HandlerParam)]
        struct UnitParam;

        #[derive(HandlerParam)]
        struct ParamWithLifetime<'a> {
            foo: &'a Handlers,
        }

        #[derive(HandlerParam)]
        struct ParamWithTwoLifetimes<'a, 'b> {
            foo: &'a Handlers,
            bar: &'b Events,
        }

        #[derive(HandlerParam)]
        struct ParamWithTypeParam<'a, T> {
            foo: &'a Handlers,
            bar: T,
        }

        #[derive(HandlerParam)]
        struct TupleStructParam<'a>(&'a Handlers, &'a Events);

        assert_handler_param::<UnitParam>();
        assert_handler_param::<ParamWithLifetime>();
        assert_handler_param::<ParamWithTwoLifetimes>();
        assert_handler_param::<ParamWithTypeParam<()>>();

        fn assert_handler_param<P: HandlerParam>() {}
    }

    #[test]
    fn handler_run_order() {
        let mut world = World::new();

        #[derive(Event)]
        struct E(#[event(target)] EntityId);

        #[derive(Component)]
        struct Tracker(String);

        fn a(_: Receiver<E, &mut Tracker>) {
            unreachable!()
        }
        fn b(r: Receiver<E, &mut Tracker>) {
            assert_eq!(r.query.0, "");
            r.query.0.push('b');
        }
        fn c(r: Receiver<E, &Tracker>) {
            assert_eq!(r.query.0, "b");
        }

        let a_id = world.add_handler(a);
        world.add_handler(b);

        world.remove_handler(a_id);

        world.add_handler(c);

        let e = world.spawn();
        world.insert(e, Tracker(String::new()));

        world.send(E(e));
    }

    #[test]
    fn handler_info_aliasing() {
        let mut world = World::new();

        #[derive(Event)]
        struct E;

        world.add_handler(|_: Receiver<E>, info: &HandlerInfo| {
            // For Miri.
            let _foo = info.name();
            let _bar = info.received_event();
            let _baz = info.referenced_components();
            let _ = format!("{info:?}");
        });

        world.send(E);
    }
}
