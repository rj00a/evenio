//! Types for sending and receiving [`Event`]s.

mod global;
mod targeted;

use alloc::borrow::Cow;
#[cfg(not(feature = "std"))]
use alloc::{vec, vec::Vec};
use core::alloc::Layout;
use core::any::TypeId;
use core::marker::PhantomData;
use core::ops::{Deref, DerefMut};
use core::panic::{RefUnwindSafe, UnwindSafe};
use core::ptr::NonNull;
use core::{any, fmt};

use bumpalo::Bump;
use evenio_macros::all_tuples;
pub use global::*;
pub use targeted::*;

use crate::access::Access;
use crate::archetype::Archetype;
use crate::assert::AssertMutable;
use crate::component::ComponentIdx;
use crate::drop::{drop_fn_of, DropFn};
use crate::entity::{EntityId, EntityLocation};
use crate::fetch::FetcherState;
use crate::handler::{HandlerConfig, HandlerInfo, HandlerParam, InitError};
use crate::prelude::Component;
use crate::query::Query;
use crate::world::{UnsafeWorldCell, World};

/// Messages which event handlers listen for.
///
/// To send and receive events within handlers, see [`Sender`] and
/// [`Receiver`].
///
/// # Targeted vs. Untargeted
///
/// Targeted events are directed at a particular entity, while untargeted events
/// may not be. For instance, the standard [`Despawn`] event is targeted because
/// it contains the [`EntityId`] of the entity it is intended to affect.
///
/// Targeted events allow handlers to efficiently filter out events whose target
/// does not match a particular query.
///
/// # Deriving
///
/// The `Event` trait is automatically implementable by using the associated
/// derive macro. Deriving via the macro is always safe.
///
/// ```
/// use evenio::prelude::*;
///
/// #[derive(Event)]
/// #[event(immutable)] // Overrides the default mutability.
/// struct MyEvent {
///     #[event(target)]
///     // Sets the entity returned by `target()`. If absent, the event is untargeted.
///     entity: EntityId,
/// }
///
/// // Also works on tuple structs, enums, and unions.
/// // However, `#[event(target)]` is unavailable for non-struct types.
///
/// #[derive(Event)]
/// struct TupleStruct(i32, #[event(target)] EntityId);
///
/// #[derive(Event)]
/// enum Enum {
///     Foo(i32),
///     Bar(f32),
/// }
///
/// #[derive(Event)]
/// union Union {
///     foo: i32,
///     bar: f32,
/// }
///
/// #[derive(Event)]
/// struct EmptyEvent;
/// ```
///
/// # Safety
///
/// This trait is `unsafe` to implement because unsafe code relies on correct
/// implementations of [`This`] and [`target`] to avoid undefined behavior. Note
/// that implementations produced by the derive macro are always safe.
///
/// [`This`]: Self::This
/// [`target`]: Self::target
pub unsafe trait Event {
    /// The type of `Self`, but with lifetimes modified to outlive `'a`.
    ///
    /// # Safety
    ///
    /// This type _must_ correspond to the type of `Self`. In particular, it
    /// must be safe to transmute between `Self` and `This<'a>` (assuming `'a`
    /// is correct). Additionally, the [`TypeId`] of `Self` must match that
    /// of `This<'static>`.
    type This<'a>: 'a;

    type EventIdx: 'static;

    // /// If this event is considered "targeted" or "untargeted".
    // ///
    // /// If `true`, [`target`] is expected to successfully return the target of
    // /// the event. Otherwise, the result of [`target`] is unspecified.
    // ///
    // /// [`target`]: Event::target
    // const IS_TARGETED: bool;

    /// Whether or not this event is considered immutable.
    ///
    /// Immutable events disallow mutable references to the event and ownership
    /// transfer via [`EventMut::take`]. This is useful for ensuring events
    /// are not altered during their lifespan.
    const IS_IMMUTABLE: bool;

    /// Gets the [`EventKind`] of this event and performs any necessary
    /// initialization work.
    ///
    /// # Safety
    ///
    /// Although this method is safe to call, it is unsafe to implement
    /// because unsafe code relies on the returned [`EventKind`] being correct
    /// for this type. Additionally, the `world` cannot be used in ways that
    /// would result in dangling indices during handler initialization.
    ///
    /// The exact safety requirements are currently unspecified, but the default
    /// implementation returns [`EventKind::Normal`] and is always safe.
    fn init(world: &mut World) -> EventKind {
        let _ = world;
        EventKind::Normal
    }
}

/// Additional behaviors for an event. This is used to distinguish normal
/// user events from special built-in events.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default, Debug)]
#[non_exhaustive]
pub enum EventKind {
    /// An event not covered by one of the other variants. Events of this kind
    /// have no special effects.
    #[default]
    Normal,
    /// The [`Insert`] event.
    Insert {
        /// The [`ComponentIdx`] of the component to insert.
        component_idx: ComponentIdx,
    },
    /// The [`Remove`] event.
    Remove {
        /// The [`ComponentIdx`] of the component to remove.
        component_idx: ComponentIdx,
    },
    /// The [`Spawn`] event.
    Spawn,
    /// The [`Despawn`] event.
    Despawn,
}

/// Data needed to create a new event.
#[derive(Clone, Debug)]
pub struct EventDescriptor {
    /// The name of this event.
    ///
    /// This name is intended for debugging purposes and should not be relied
    /// upon for correctness.
    pub name: Cow<'static, str>,
    /// The [`TypeId`] of this event, if any.
    pub type_id: Option<TypeId>,
    /// The [`EventKind`] of the event.
    pub kind: EventKind,
    /// The [`Layout`] of the event.
    pub layout: Layout,
    /// The [`DropFn`] of the event. This is passed a pointer to the
    /// event in order to drop it.
    pub drop: DropFn,
    /// If this event is [immutable](Event::IS_IMMUTABLE).
    pub is_immutable: bool,
}

impl EventDescriptor {
    pub(crate) fn new<E: Event>(world: &mut World) -> Self {
        Self {
            name: any::type_name::<E>().into(),
            type_id: Some(TypeId::of::<E::This<'static>>()),
            kind: E::init(world),
            layout: Layout::new::<E>(),
            drop: drop_fn_of::<E>(),
            is_immutable: E::IS_IMMUTABLE,
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum EventId {
    Global(GlobalEventId),
    Targeted(TargetedEventId),
}

impl EventId {
    pub const fn is_global(self) -> bool {
        matches!(self, Self::Global(_))
    }

    pub const fn is_targeted(self) -> bool {
        matches!(self, Self::Targeted(_))
    }
}

impl From<GlobalEventId> for EventId {
    fn from(value: GlobalEventId) -> Self {
        Self::Global(value)
    }
}

impl From<TargetedEventId> for EventId {
    fn from(value: TargetedEventId) -> Self {
        Self::Targeted(value)
    }
}

#[derive(Debug)]
pub(crate) struct EventQueue {
    items: Vec<EventQueueItem>,
    bump: Bump,
}

impl EventQueue {
    pub(crate) fn new() -> Self {
        Self {
            items: vec![],
            bump: Bump::new(),
        }
    }

    pub(crate) fn pop_front(&mut self) -> Option<EventQueueItem> {
        self.items.pop()
    }

    #[inline]
    pub(crate) unsafe fn push_front_global<E: Event>(&mut self, event: E, idx: GlobalEventIdx) {
        let meta = EventMeta::Global { idx };
        let event = NonNull::from(self.bump.alloc(event)).cast::<u8>();
        self.items.push(EventQueueItem { meta, event });
    }

    #[inline]
    pub(crate) unsafe fn push_front_targeted<E: Event>(
        &mut self,
        target: EntityId,
        event: E,
        idx: TargetedEventIdx,
    ) {
        let meta = EventMeta::Targeted { idx, target };
        let event = NonNull::from(self.bump.alloc(event)).cast::<u8>();
        self.items.push(EventQueueItem { meta, event });
    }

    /// Reverses elements in the range `from..`.
    ///
    /// # Safety
    ///
    /// `from` must be in bounds.
    pub(crate) unsafe fn reverse_from(&mut self, from: usize) {
        self.items.get_unchecked_mut(from..).reverse();
    }

    pub(crate) fn iter(&self) -> impl Iterator<Item = &EventQueueItem> {
        self.items.iter()
    }

    /// Clears the event queue and resets the internal bump allocator.
    ///
    /// Any remaining event pointers are invalidated.
    pub(crate) fn clear(&mut self) {
        self.items.clear();
        self.bump.reset();
    }

    pub(crate) fn len(&self) -> usize {
        self.items.len()
    }

    pub(crate) fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

impl UnwindSafe for EventQueue {}
impl RefUnwindSafe for EventQueue {}

#[derive(Clone, Copy, Debug)]
pub(crate) struct EventQueueItem {
    pub(crate) meta: EventMeta,
    /// Type-erased pointer to this event. When null, ownership of the event
    /// has been transferred and no destructor needs to run.
    pub(crate) event: NonNull<u8>,
}

/// Metadata for an event in the event queue.
#[derive(Clone, Copy, Debug)]
pub(crate) enum EventMeta {
    Global {
        idx: GlobalEventIdx,
    },
    Targeted {
        idx: TargetedEventIdx,
        target: EntityId,
    },
}

/// Type-erased pointer to an event. Passed to handlers in [`Handler::run`].
///
/// [`Handler::run`]: crate::handler::Handler::run
#[derive(Clone, Copy, Debug)]
pub struct EventPtr<'a> {
    event: NonNull<u8>,
    // `false` when borrowed, `true` when taken.
    ownership_flag: NonNull<bool>,
    _marker: PhantomData<&'a mut u8>,
}

impl<'a> EventPtr<'a> {
    pub(crate) fn new(event: NonNull<u8>, ownership_flag: NonNull<bool>) -> Self {
        Self {
            event,
            ownership_flag,
            _marker: PhantomData,
        }
    }

    /// Returns the underlying pointer to the type-erased event.
    #[track_caller]
    pub fn as_ptr(self) -> NonNull<u8> {
        let is_owned = unsafe { *self.ownership_flag.as_ptr() };
        debug_assert!(
            !is_owned,
            "`as_ptr` cannot be called after the event has been marked as owned"
        );

        self.event
    }

    /// Marks the event as owned. It is then the handler's responsibility to
    /// drop the event.
    ///
    /// # Safety
    ///
    /// - Must have permission to access the event mutably.
    /// - Once the event is set as owned, [`as_ptr`] cannot be called.
    ///
    /// [`as_ptr`]: Self::as_ptr
    pub unsafe fn set_owned(self) {
        *self.ownership_flag.as_ptr() = true;
    }
}

/// Mutable reference to an instance of event `E`.
///
/// To get at `E`, use the [`Deref`] and [`DerefMut`] implementations or
/// [`take`](Self::take).
pub struct EventMut<'a, E: Event> {
    ptr: EventPtr<'a>,
    _marker: PhantomData<&'a mut E::This<'a>>,
}

impl<'a, E: Event> EventMut<'a, E> {
    fn new(ptr: EventPtr<'a>) -> Self {
        Self {
            ptr,
            _marker: PhantomData,
        }
    }

    /// Takes ownership of the event. Any other handlers listening for this
    /// event will not run.
    ///
    /// # Examples
    ///
    /// ```rust
    /// # use evenio::prelude::*;
    /// # let mut world = World::new();
    /// # #[derive(Event)]
    /// # struct E;
    /// #
    /// # let mut world = World::new();
    /// #
    /// world.add_handler(|r: ReceiverMut<E>| {
    ///     EventMut::take(r.event); // Took ownership of event.
    /// });
    ///
    /// world.add_handler(|_: Receiver<E>| panic!("boom"));
    ///
    /// world.send(E);
    /// // ^ No panic occurs because the first handler took
    /// // ownership of the event before the second could run.
    /// ```
    pub fn take(this: Self) -> E {
        let res = unsafe { this.ptr.as_ptr().as_ptr().cast::<E>().read() };
        unsafe { this.ptr.set_owned() };
        res
    }
}

unsafe impl<'a, E> Send for EventMut<'a, E>
where
    E: Event,
    E::This<'a>: Send,
{
}

unsafe impl<'a, E> Sync for EventMut<'a, E>
where
    E: Event,
    E::This<'a>: Sync,
{
}

impl<'a, E: Event> Deref for EventMut<'a, E> {
    type Target = E::This<'a>;

    fn deref(&self) -> &Self::Target {
        unsafe { self.ptr.as_ptr().cast::<E::This<'_>>().as_ref() }
    }
}

impl<E: Event> DerefMut for EventMut<'_, E> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { self.ptr.as_ptr().cast::<E::This<'_>>().as_mut() }
    }
}

impl<'a, E> fmt::Debug for EventMut<'a, E>
where
    E: Event,
    E::This<'a>: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("EventMut").field(&**self).finish()
    }
}

/// A [`HandlerParam`] which listens for events of type `E`.
///
/// For more information, see the relevant [tutorial
/// chapter](crate::tutorial::ch01_handlers_and_events#handlers-and-events).
///
/// # Examples
///
/// ```
/// use evenio::prelude::*;
///
/// #[derive(Event)]
/// struct E;
///
/// let mut world = World::new();
///
/// world.add_handler(|r: Receiver<E>| {
///     println!("got event of type E!");
/// });
/// ```
#[derive(Clone, Copy)]
pub struct Receiver<'a, E: Event, Q: ReceiverQuery + 'static = NullReceiverQuery> {
    /// A reference to the received event.
    pub event: &'a E::This<'a>,
    /// The result of the query. This field is meaningless if `E` is not a
    /// targeted event.
    pub query: Q::Item<'a>,
}

unsafe impl<E: GlobalEvent> HandlerParam for Receiver<'_, E> {
    type State = ();

    type This<'a> = Receiver<'a, E>;

    fn init(world: &mut World, config: &mut HandlerConfig) -> Result<Self::State, InitError> {
        let event_id = world.add_global_event::<E>();

        config.set_received_event(event_id);
        config.set_received_event_access(Access::Read);

        Ok(())
    }

    unsafe fn get<'a>(
        _state: &'a mut Self::State,
        _info: &'a HandlerInfo,
        event_ptr: EventPtr<'a>,
        _target_location: EntityLocation,
        _world: UnsafeWorldCell<'a>,
    ) -> Self::This<'a> {
        Receiver {
            // SAFETY:
            // - We have permission to access the event immutably.
            // - Handler was configured to listen for `E`.
            event: event_ptr.as_ptr().cast().as_ref(),
            query: (),
        }
    }

    fn refresh_archetype(_state: &mut Self::State, _arch: &Archetype) {}

    fn remove_archetype(_state: &mut Self::State, _arch: &Archetype) {}
}

impl<'a, E> fmt::Debug for Receiver<'a, E>
where
    E: GlobalEvent,
    E::This<'a>: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Receiver")
            .field("event", &self.event)
            .finish_non_exhaustive()
    }
}

unsafe impl<E: TargetedEvent, Q: Query + 'static> HandlerParam for Receiver<'_, E, Q> {
    type State = FetcherState<Q>;

    type This<'a> = Receiver<'a, E, Q>;

    fn init(world: &mut World, config: &mut HandlerConfig) -> Result<Self::State, InitError> {
        let event_id = world.add_targeted_event::<E>();

        let (ca, state) = Q::init(world, config)?;

        config.set_received_event(event_id);
        config.set_received_event_access(Access::Read);
        config.set_targeted_event_component_access(ca.clone());
        config.push_component_access(ca);

        Ok(FetcherState::new(state))
    }

    unsafe fn get<'a>(
        state: &'a mut Self::State,
        _info: &'a HandlerInfo,
        event_ptr: EventPtr<'a>,
        target_location: EntityLocation,
        _world: UnsafeWorldCell<'a>,
    ) -> Self::This<'a> {
        let event = event_ptr.as_ptr().cast::<E::This<'_>>().as_ref();

        // SAFETY: Caller guarantees the target entity matches the query.
        let query = state.get_by_location_mut(target_location);

        Receiver { event, query }
    }

    fn refresh_archetype(state: &mut Self::State, arch: &Archetype) {
        state.refresh_archetype(arch)
    }

    fn remove_archetype(state: &mut Self::State, arch: &Archetype) {
        state.remove_archetype(arch)
    }
}

impl<'a, E, Q> fmt::Debug for Receiver<'a, E, Q>
where
    E: TargetedEvent,
    E::This<'a>: fmt::Debug,
    Q: Query,
    Q::Item<'a>: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Receiver")
            .field("event", &self.event)
            .field("query", &self.query)
            .finish()
    }
}

/// Like [`Receiver`], but provides mutable access to the received event. Prefer
/// `Receiver` if mutable access is not needed.
///
/// For more information, see the relevant [tutorial
/// chapter](crate::tutorial::ch02_event_mutation).
pub struct ReceiverMut<'a, E: Event, Q: ReceiverQuery + 'static = NullReceiverQuery> {
    /// A mutable reference to the received event.
    pub event: EventMut<'a, E>,
    /// The result of the query. This field is meaningless if `E` is not a
    /// targeted event.
    pub query: Q::Item<'a>,
}

unsafe impl<E: GlobalEvent> HandlerParam for ReceiverMut<'_, E> {
    type State = ();

    type This<'a> = ReceiverMut<'a, E>;

    fn init(world: &mut World, config: &mut HandlerConfig) -> Result<Self::State, InitError> {
        let () = AssertMutable::<E>::EVENT; // TODO: Remove this.

        let event_id = world.add_global_event::<E>();

        config.set_received_event(event_id);
        config.set_received_event_access(Access::ReadWrite);

        Ok(())
    }

    unsafe fn get<'a>(
        _state: &'a mut Self::State,
        _info: &'a HandlerInfo,
        event_ptr: EventPtr<'a>,
        _target_location: EntityLocation,
        _world: UnsafeWorldCell<'a>,
    ) -> Self::This<'a> {
        ReceiverMut {
            event: EventMut::new(event_ptr),
            query: (),
        }
    }

    fn refresh_archetype(_state: &mut Self::State, _arch: &Archetype) {}

    fn remove_archetype(_state: &mut Self::State, _arch: &Archetype) {}
}

impl<'a, E> fmt::Debug for ReceiverMut<'a, E>
where
    E: GlobalEvent,
    E::This<'a>: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Receiver")
            .field("event", &self.event)
            .finish_non_exhaustive()
    }
}

unsafe impl<E: TargetedEvent, Q: Query + 'static> HandlerParam for ReceiverMut<'_, E, Q> {
    type State = FetcherState<Q>;

    type This<'a> = ReceiverMut<'a, E, Q>;

    fn init(world: &mut World, config: &mut HandlerConfig) -> Result<Self::State, InitError> {
        let () = AssertMutable::<E>::EVENT; // TODO: Remove this.

        let event_id = world.add_targeted_event::<E>();

        let (ca, state) = Q::init(world, config)?;

        config.set_received_event(event_id);
        config.set_received_event_access(Access::ReadWrite);
        config.set_targeted_event_component_access(ca.clone());
        config.push_component_access(ca);

        Ok(FetcherState::new(state))
    }

    unsafe fn get<'a>(
        state: &'a mut Self::State,
        _info: &'a HandlerInfo,
        event_ptr: EventPtr<'a>,
        target_location: EntityLocation,
        _world: UnsafeWorldCell<'a>,
    ) -> Self::This<'a> {
        let event = EventMut::<E>::new(event_ptr);

        // SAFETY: Caller guarantees the target entity matches the query.
        let query = state.get_by_location_mut(target_location);

        ReceiverMut { event, query }
    }

    fn refresh_archetype(state: &mut Self::State, arch: &Archetype) {
        state.refresh_archetype(arch)
    }

    fn remove_archetype(state: &mut Self::State, arch: &Archetype) {
        state.remove_archetype(arch)
    }
}

impl<'a, E, Q> fmt::Debug for ReceiverMut<'a, E, Q>
where
    E: TargetedEvent,
    E::This<'a>: fmt::Debug,
    Q: Query,
    Q::Item<'a>: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Receiver")
            .field("event", &self.event)
            .field("query", &self.query)
            .finish()
    }
}

/// Indicates the absence of a [`ReceiverQuery`].
#[derive(Clone, Copy, Debug)]
pub struct NullReceiverQuery;

/// Targeted event queries used in [`Receiver`] and [`ReceiverMut`]. This trait
/// is implemented for all types which implement [`Query`].
///
/// This trait is sealed and cannot be implemented for types outside this crate.
pub trait ReceiverQuery: private::Sealed {
    /// The item produced by the query.
    type Item<'a>;
}

impl ReceiverQuery for NullReceiverQuery {
    type Item<'a> = ();
}

impl<Q: Query> ReceiverQuery for Q {
    type Item<'a> = Q::Item<'a>;
}

mod private {
    use super::*;

    pub trait Sealed {}

    impl Sealed for NullReceiverQuery {}

    impl<Q: Query> Sealed for Q {}
}

/// A [`HandlerParam`] for sending events from the set `T`.
///
/// For more information, see the relevant [tutorial
/// chapter](crate::tutorial::ch03_sending_events_from_handlers).
#[derive(Clone, Copy)]
pub struct Sender<'a, T: EventSet> {
    state: &'a T::Indices,
    world: UnsafeWorldCell<'a>,
}

impl<T: EventSet> Sender<'_, T> {
    /// Add an untargeted [`Event`] to the queue of events to send. The queue is
    /// flushed once the handler returns.
    ///
    /// # Panics
    ///
    /// - Panics if `E` is not in the [`EventSet`] of this sender.
    /// - Panics at compile-time if `E` is [targeted](Event::IS_TARGETED).
    #[track_caller]
    pub fn send<E: GlobalEvent + 'static>(&mut self, event: E) {
        // The event type and event set are all compile time known, so the compiler
        // should be able to optimize this away.
        let event_idx = T::find_index::<E>(self.state).unwrap_or_else(|| {
            panic!(
                "event `{}` is not in the `EventSet` of this `Sender`",
                any::type_name::<E>()
            )
        });

        unsafe { self.world.send_global(event, GlobalEventIdx(event_idx)) }
    }

    #[track_caller]
    pub fn send_to<E: Event + 'static>(&mut self, target: EntityId, event: E) {
        // The event type and event set are all compile time known, so the compiler
        // should be able to optimize this away.
        let event_idx = T::find_index::<E>(self.state).unwrap_or_else(|| {
            panic!(
                "event `{}` is not in the `EventSet` of this `Sender`",
                any::type_name::<E>()
            )
        });

        unsafe {
            self.world
                .send_targeted(target, event, TargetedEventIdx(event_idx))
        }
    }

    /// Queues an entity to be spawned, returns its [`EntityId`], and queues the
    /// [`Spawn`] event. The returned `EntityId` is not used by any previous
    /// entities in the [`World`].
    ///
    /// The entity will not exist in the world until the `Spawn` event has
    /// started broadcasting.
    ///
    /// # Panics
    ///
    /// Panics if `Spawn` is not in the [`EventSet`] of this sender.
    #[track_caller]
    pub fn spawn(&mut self) -> EntityId {
        let id = unsafe { self.world.queue_spawn() };
        self.send(Spawn(id));
        id
    }

    /// Queue an [`Insert`] event.
    ///
    /// This is equivalent to:
    ///
    /// ```
    /// # use evenio::prelude::*;
    /// # let mut world = World::new();
    /// # #[derive(Event)]
    /// # struct E;
    /// # #[derive(Component)]
    /// # struct C;
    /// # world.add_handler(|_: Receiver<E>, mut sender: Sender<Insert<C>>| {
    /// #     let entity = EntityId::NULL;
    /// #     let component = C;
    /// sender.send(Insert::new(entity, component));
    /// # });
    /// ```
    ///
    /// # Panics
    ///
    /// Panics if `Insert<C>` is not in the [`EventSet`] of this sender.
    #[track_caller]
    pub fn insert<C: Component>(&mut self, entity: EntityId, component: C) {
        self.send_to(entity, Insert(component))
    }

    /// Queue a [`Remove`] event.
    ///
    /// This is equivalent to:
    ///
    /// ```
    /// # use evenio::prelude::*;
    /// # let mut world = World::new();
    /// # let entity = world.spawn();
    /// # #[derive(Event)] struct E;
    /// # #[derive(Component)] struct C;
    /// # world.add_handler(move |_: Receiver<E>, mut sender: Sender<Remove<C>>| {
    /// sender.send(Remove::<C>::new(entity));
    /// # });
    /// ```
    ///
    /// # Panics
    ///
    /// Panics if `Remove<C>` is not in the [`EventSet`] of this sender.
    #[track_caller]
    pub fn remove<C: Component>(&mut self, entity: EntityId) {
        self.send_to(entity, Remove::<C>)
    }

    /// Queue a [`Despawn`] event.
    ///
    /// This is equivalent to:
    ///
    /// ```
    /// # use evenio::prelude::*;
    /// # let mut world = World::new();
    /// # let entity = world.spawn();
    /// # #[derive(Event)] struct E;
    /// # world.add_handler(move |_: Receiver<E>, mut sender: Sender<Despawn>| {
    /// sender.send(Despawn(entity));
    /// # });
    /// ```
    ///
    /// # Panics
    ///
    /// Panics if `Despawn` is not in the [`EventSet`] of this sender.
    #[track_caller]
    pub fn despawn(&mut self, entity: EntityId) {
        self.send_to(entity, Despawn)
    }
}

unsafe impl<T: EventSet> HandlerParam for Sender<'_, T> {
    type State = T::Indices;

    type This<'a> = Sender<'a, T>;

    fn init(world: &mut World, config: &mut HandlerConfig) -> Result<Self::State, InitError> {
        config.set_event_queue_access(Access::ReadWrite);

        let state = T::new_indices(world);

        T::for_each_index(&state, |is_targeted, idx| {
            // TODO: config.insert_sent_event
            if is_targeted {
                config.sent_targeted_events.insert(TargetedEventIdx(idx));
            } else {
                config.sent_global_events.insert(GlobalEventIdx(idx));
            }
        });

        Ok(state)
    }

    unsafe fn get<'a>(
        state: &'a mut Self::State,
        _info: &'a HandlerInfo,
        _event_ptr: EventPtr<'a>,
        _target_location: EntityLocation,
        world: UnsafeWorldCell<'a>,
    ) -> Self::This<'a> {
        Sender { state, world }
    }

    fn refresh_archetype(_state: &mut Self::State, _arch: &Archetype) {}

    fn remove_archetype(_state: &mut Self::State, _arch: &Archetype) {}
}

impl<T: EventSet> fmt::Debug for Sender<'_, T>
where
    T::Indices: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Sender")
            .field("state", &self.state)
            .field("world", &self.world)
            .finish()
    }
}

/// A set of [`Event`] types.
///
/// This trait is implemented for all events and tuples of events, so `E1`,
/// `()`, `(E1,)`, `(E1, E2)` etc. are all event sets.
///
/// # Safety
///
/// This trait is marked `unsafe` because unsafe code relies on implementations
/// being correct. It is not recommended to implement this trait yourself.
pub unsafe trait EventSet {
    /// The set of event indices.
    type Indices: 'static;

    fn new_indices(world: &mut World) -> Self::Indices;

    fn find_index<F: Event>(indices: &Self::Indices) -> Option<u32>;

    fn for_each_index<F: FnMut(bool, u32)>(indices: &Self::Indices, f: F);
}

unsafe impl<E: Event> EventSet for E {
    type Indices = u32;

    fn new_indices(world: &mut World) -> Self::Indices {
        let desc = EventDescriptor::new::<E>(world);

        if TypeId::of::<E::EventIdx>() == TypeId::of::<TargetedEventIdx>() {
            unsafe { world.add_targeted_event_with_descriptor(desc) }
                .index()
                .0
        } else {
            unsafe { world.add_targeted_event_with_descriptor(desc) }
                .index()
                .0
        }
    }

    #[inline]
    fn find_index<F: Event>(index: &Self::Indices) -> Option<u32> {
        (TypeId::of::<E::This<'static>>() == TypeId::of::<F::This<'static>>()).then_some(*index)
    }

    fn for_each_index<F: FnMut(bool, u32)>(index: &Self::Indices, mut f: F) {
        f(
            TypeId::of::<E::EventIdx>() == TypeId::of::<TargetedEventIdx>(),
            *index,
        )
    }
}

macro_rules! impl_event_set_tuple {
    ($(($E:ident, $e:ident)),*) => {
        #[allow(unused_variables, unused_mut, clippy::unused_unit)]
        unsafe impl<$($E: EventSet),*> EventSet for ($($E,)*) {
            type Indices = ($($E::Indices,)*);

            fn new_indices(world: &mut World) -> Self::Indices {
                (
                    $(
                        $E::new_indices(world),
                    )*
                )
            }

            #[inline]
            fn find_index<F: Event>(($($e,)*): &Self::Indices) -> Option<u32> {
                $(
                    if let Some(id) = $E::find_index::<F>($e) {
                        return Some(id);
                    }
                )*

                None
            }

            fn for_each_index<F: FnMut(bool, u32)>(($($e,)*): &Self::Indices, mut f: F) {
                $(
                    $E::for_each_index($e, &mut f);
                )*
            }
        }
    };
}

all_tuples!(impl_event_set_tuple, 0, 15, E, e);

/// An [`Event`] which adds component `C` on an entity when sent. If the entity
/// already has the component, then the component is replaced.
///
/// Any handler which listens for `Insert<C>` will run before the component is
/// inserted. `Insert<C>` has no effect if the target entity does not exist or
/// the event is consumed before it finishes broadcasting.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
#[repr(transparent)]
pub struct Insert<C>(pub C);

unsafe impl<C: Component> Event for Insert<C> {
    type This<'a> = Insert<C>;

    type EventIdx = TargetedEventIdx;

    const IS_IMMUTABLE: bool = false;

    fn init(world: &mut World) -> EventKind {
        EventKind::Insert {
            component_idx: world.add_component::<C>().index(),
        }
    }
}

impl<C> Deref for Insert<C> {
    type Target = C;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<C> DerefMut for Insert<C> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

/// An [`Event`] which removes component `C` from an entity when sent. The
/// component is dropped and cannot be recovered.
///
/// Any handler which listens for `Remove<C>` will run before the component is
/// removed. `Remove<C>` has no effect if the target entity does not exist or
/// the event is consumed before it finishes broadcasting.
#[ghost::phantom]
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct Remove<C>;

unsafe impl<C: Component> Event for Remove<C> {
    type This<'a> = Remove<C>;

    type EventIdx = TargetedEventIdx;

    const IS_IMMUTABLE: bool = false;

    fn init(world: &mut World) -> EventKind {
        EventKind::Remove {
            component_idx: world.add_component::<C>().index(),
        }
    }
}

/// An [`Event`] which signals the creation of an entity. Contains the
/// [`EntityId`] of the new entity, which may or may not exist by the time this
/// event is observed.
///
/// Note that the event by itself cannot be used to spawn new entities. Use
/// [`World::spawn`] or [`Sender::spawn`] instead.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
#[repr(transparent)]
pub struct Spawn(pub EntityId);

unsafe impl Event for Spawn {
    type This<'a> = Self;

    type EventIdx = GlobalEventIdx;

    const IS_IMMUTABLE: bool = true;

    fn init(_world: &mut World) -> EventKind {
        EventKind::Spawn
    }
}

/// An [`Event`] which removes an entity from the [`World`] when sent. All
/// components of the target entity are dropped.
///
/// Any handler which listens for `Despawn` will run before the entity is
/// removed. `Despawn` has no effect if the target entity does not exist or the
/// event is consumed before it finishes broadcasting.
///
/// # Examples
///
/// ```
/// use evenio::prelude::*;
///
/// let mut world = World::new();
///
/// let id = world.spawn();
///
/// assert!(world.entities().contains(id));
///
/// world.add_handler(|r: Receiver<Despawn, ()>| {
///     println!("{:?} is about to despawn!", r.event.0);
/// });
///
/// world.send(Despawn(id));
///
/// assert!(!world.entities().contains(id));
/// ```
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct Despawn;

unsafe impl Event for Despawn {
    type This<'a> = Despawn;

    type EventIdx = TargetedEventIdx;

    const IS_IMMUTABLE: bool = false;

    fn init(_world: &mut World) -> EventKind {
        EventKind::Despawn
    }
}

#[cfg(test)]
mod tests {
    use rand::prelude::*;

    use crate::prelude::*;

    #[test]
    fn change_entity_during_broadcast() {
        let mut world = World::new();

        #[derive(TargetedEvent)]
        struct E;

        #[derive(Component)]
        struct C(String);

        world.add_handler(|r: Receiver<E, EntityId>, mut s: Sender<Remove<C>>| {
            s.remove::<C>(r.query);
        });

        world.add_handler(|r: Receiver<E, &mut C>| {
            r.query.0.push_str("123");
        });

        let e = world.spawn();
        world.insert(e, C("abc".into()));

        world.send_to(e, E);
    }

    #[test]
    fn event_order() {
        #[derive(GlobalEvent)]
        struct A;
        #[derive(GlobalEvent, Debug)]
        struct B(i32);
        #[derive(GlobalEvent, Debug)]
        struct C(i32);

        #[derive(Component)]
        struct Result(Vec<i32>);

        fn get_a_send_b(_: Receiver<A>, mut sender: Sender<B>) {
            sender.send(B(0));
            sender.send(B(3));
        }

        fn get_b_send_c(r: Receiver<B>, mut sender: Sender<C>, res: Single<&mut Result>) {
            res.0 .0.push(r.event.0);
            sender.send(C(r.event.0 + 1));
            sender.send(C(r.event.0 + 2));
        }

        fn get_c(r: Receiver<C>, res: Single<&mut Result>) {
            res.0 .0.push(r.event.0);
        }

        let mut world = World::new();

        let res = world.spawn();
        world.insert(res, Result(vec![]));

        world.add_handler(get_a_send_b);
        world.add_handler(get_b_send_c);
        world.add_handler(get_c);

        world.send(A);

        assert_eq!(
            world.get::<Result>(res).unwrap().0.as_slice(),
            &[0, 1, 2, 3, 4, 5]
        );
    }

    #[test]
    fn despawn_many() {
        let mut world = World::new();

        #[derive(GlobalEvent)]
        struct E;

        #[derive(Component)]
        struct C(#[allow(unused)] i32);

        let mut entities = vec![];

        let n = 50;

        for i in 0..n {
            let e = world.spawn();
            world.insert(e, C(i));
            entities.push(e);
        }

        entities.shuffle(&mut rand::thread_rng());

        world.add_handler(move |_: Receiver<E>, mut s: Sender<Despawn>| {
            for &e in &entities {
                s.despawn(e);
            }
        });

        world.send(E);

        assert_eq!(world.entities().len(), 0);
    }

    #[test]
    fn send_borrowed() {
        let mut buf = [1, 2, 3];

        #[derive(GlobalEvent, Debug)]
        struct A<'a>(&'a mut [i32]);

        impl Drop for A<'_> {
            fn drop(&mut self) {
                for item in self.0.iter_mut() {
                    *item *= 2;
                    println!("{item}");
                }
            }
        }

        let mut world = World::new();

        world.add_handler(|r: Receiver<A>| println!("{r:?}"));

        world.send(A(&mut buf));
    }
}
