//! Types for sending and receiving [`Event`]s.

use alloc::borrow::Cow;
use alloc::vec::Vec;
use alloc::{format, vec};
use core::alloc::Layout;
use core::any::TypeId;
use core::marker::PhantomData;
use core::num::NonZeroU32;
use core::ops::{Deref, DerefMut, Index};
use core::panic::{RefUnwindSafe, UnwindSafe};
use core::ptr::NonNull;
use core::{any, fmt};

use bumpalo::Bump;
use evenio_macros::all_tuples;
pub use evenio_macros::Event;
use memoffset::offset_of;

use crate::access::Access;
use crate::archetype::Archetype;
use crate::assert::{
    AssertMutable, AssertTargetedEvent, AssertUntargetedEvent, GetDebugChecked, UnwrapDebugChecked,
};
use crate::component::ComponentIdx;
use crate::drop::DropFn;
use crate::entity::EntityId;
use crate::fetch::FetcherState;
use crate::map::{Entry, TypeIdMap};
use crate::prelude::Component;
use crate::query::Query;
use crate::slot_map::{Key, SlotMap};
use crate::sparse::SparseIndex;
use crate::system::{Config, InitError, SystemInfo, SystemParam};
use crate::world::{UnsafeWorldCell, World};

/// Stores metadata for all [`Event`]s in the world.
///
/// This can be obtained in a system by using the `&Events` system
/// parameter.
///
/// ```
/// # use evenio::prelude::*;
/// # use evenio::event::Events;
/// #
/// # #[derive(Event)] struct E;
/// #
/// # let mut world = World::new();
/// world.add_system(|_: Receiver<E>, events: &Events| {});
#[derive(Debug)]
pub struct Events {
    untargeted_events: SlotMap<EventInfo>,
    targeted_events: SlotMap<EventInfo>,
    by_type_id: TypeIdMap<EventId>,
}

impl Events {
    pub(crate) fn new() -> Self {
        let mut this = Self {
            untargeted_events: SlotMap::new(),
            targeted_events: SlotMap::new(),
            by_type_id: TypeIdMap::default(),
        };

        this.add(EventDescriptor {
            name: any::type_name::<SpawnQueued>().into(),
            type_id: None,
            is_targeted: false,
            kind: EventKind::SpawnQueued,
            layout: Layout::new::<SpawnQueued>(),
            drop: None,
            is_immutable: true,
        });

        this
    }

    pub(crate) fn add(&mut self, desc: EventDescriptor) -> (EventId, bool) {
        let info = EventInfo {
            id: EventId::NULL,
            name: desc.name,
            kind: desc.kind,
            type_id: desc.type_id,
            layout: desc.layout,
            drop: desc.drop,
            is_immutable: desc.is_immutable,
        };

        let insert = || {
            let map = if desc.is_targeted {
                &mut self.targeted_events
            } else {
                &mut self.untargeted_events
            };

            let Some(k) = map.insert(info) else {
                panic!("too many events")
            };
            let id = EventId::from_key(k, desc.is_targeted);
            map[k].id = id;

            id
        };

        if let Some(type_id) = desc.type_id {
            match self.by_type_id.entry(type_id) {
                Entry::Vacant(v) => (*v.insert(insert()), true),
                Entry::Occupied(o) => (*o.get(), false),
            }
        } else {
            (insert(), false)
        }
    }

    /// Gets the [`EventInfo`] of the given event. Returns `None` if the ID is
    /// invalid.
    pub fn get(&self, id: EventId) -> Option<&EventInfo> {
        let k = id.as_key();
        match id.index() {
            EventIdx::Targeted(_) => self.targeted_events.get(k),
            EventIdx::Untargeted(_) => self.untargeted_events.get(k),
        }
    }

    /// Gets the [`EventInfo`] for an event using its [`EventIdx`]. Returns
    /// `None` if the index is invalid.
    #[inline]
    pub fn get_by_index(&self, idx: EventIdx) -> Option<&EventInfo> {
        match idx {
            EventIdx::Untargeted(idx) => Some(self.untargeted_events.get_by_index(idx.0)?.1),
            EventIdx::Targeted(idx) => Some(self.targeted_events.get_by_index(idx.0)?.1),
        }
    }

    /// Gets the [`EventInfo`] for an event using its [`TypeId`]. Returns `None`
    /// if the `TypeId` does not map to an event.
    pub fn get_by_type_id(&self, type_id: TypeId) -> Option<&EventInfo> {
        debug_assert_ne!(type_id, TypeId::of::<SpawnQueued>());

        let idx = *self.by_type_id.get(&type_id)?;
        Some(unsafe { self.get(idx).unwrap_debug_checked() })
    }

    /// Does the given event exist in the world?
    pub fn contains(&self, id: EventId) -> bool {
        self.get(id).is_some()
    }

    pub(crate) fn remove(&mut self, id: EventId) -> Option<EventInfo> {
        debug_assert_ne!(id, EventId::SPAWN_QUEUED);

        let k = id.as_key();

        let info = if id.is_targeted() {
            self.targeted_events.remove(k)
        } else {
            self.untargeted_events.remove(k)
        }?;

        if let Some(type_id) = info.type_id {
            self.by_type_id.remove(&type_id);
        }

        Some(info)
    }

    /// Returns an iterator over all event infos.
    pub fn iter(&self) -> impl Iterator<Item = &EventInfo> {
        self.targeted_events
            .iter()
            .chain(self.untargeted_events.iter())
            .map(|(_, v)| v)
    }
}

impl Index<EventId> for Events {
    type Output = EventInfo;

    fn index(&self, index: EventId) -> &Self::Output {
        if let Some(info) = self.get(index) {
            info
        } else {
            panic!("no such event with ID of {index:?} exists")
        }
    }
}

impl Index<EventIdx> for Events {
    type Output = EventInfo;

    fn index(&self, index: EventIdx) -> &Self::Output {
        if let Some(info) = self.get_by_index(index) {
            info
        } else {
            panic!("no such event with index of {index:?} exists")
        }
    }
}

impl Index<TypeId> for Events {
    type Output = EventInfo;

    fn index(&self, index: TypeId) -> &Self::Output {
        if let Some(info) = self.get_by_type_id(index) {
            info
        } else {
            panic!("no such event with type ID of {index:?} exists")
        }
    }
}

unsafe impl SystemParam for &'_ Events {
    type State = ();

    type Item<'a> = &'a Events;

    fn init(_world: &mut World, _config: &mut Config) -> Result<Self::State, InitError> {
        Ok(())
    }

    unsafe fn get<'a>(
        _state: &'a mut Self::State,
        _info: &'a SystemInfo,
        _event_ptr: EventPtr<'a>,
        world: UnsafeWorldCell<'a>,
    ) -> Self::Item<'a> {
        world.events()
    }

    fn refresh_archetype(_state: &mut Self::State, _arch: &Archetype) {}

    fn remove_archetype(_state: &mut Self::State, _arch: &Archetype) {}
}
/// Messages which systems listen for.
///
/// To send and receive events within systems, see [`Sender`] and
/// [`Receiver`].
///
/// # Targeted vs. Untargeted
///
/// Targeted events are directed at a particular entity, while untargeted events
/// may not be. For instance, the standard [`Despawn`] event is targeted because
/// it contains the [`EntityId`] of the entity it is intended to affect.
///
/// Targeted events allow systems to efficiently filter out events whose target
/// does not match a particular query.
///
/// # Deriving
///
/// The `Event` trait is automatically implementable by using the associated
/// derive macro. The type must still satisfy the `Send + Sync + 'static` bound
/// to do so.
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
pub trait Event: Send + Sync + 'static {
    /// If this event is considered "targeted" or "untargeted".
    ///
    /// If `true`, [`target`] is expected to successfully return the target of
    /// the event. Otherwise, the result of [`target`] is unspecified.
    ///
    /// [`target`]: Event::target
    const IS_TARGETED: bool = false;

    /// Whether or not this event is considered immutable.
    ///
    /// Immutable events disallow mutable references to the event and ownership
    /// transfer via [`EventMut::take`]. This is useful for ensuring events
    /// are not altered during their lifespan.
    const IS_IMMUTABLE: bool = false;

    /// Returns the [`EntityId`] target of this event.
    ///
    /// If [`IS_TARGETED`] is `false`, then the result is unspecified. The
    /// default implementation panics.
    ///
    /// [`IS_TARGETED`]: Event::IS_TARGETED
    fn target(&self) -> EntityId {
        unimplemented!()
    }

    /// Gets the [`EventKind`] of this event and performs any necessary
    /// initialization work.
    ///
    /// # Safety
    ///
    /// Although this method is safe to call, it is unsafe to implement
    /// because unsafe code relies on the returned [`EventKind`] being correct
    /// for this type. Additionally, the `world` cannot be used in ways that
    /// would result in dangling indices.
    ///
    /// The exact safety requirements are currently unspecified, but the default
    /// implementation returns [`EventKind::Normal`] and is always safe.
    #[doc(hidden)]
    unsafe fn init(world: &mut World) -> EventKind {
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
        /// Cached offset from the beginning of the event to the
        /// [`Insert::component`] field.
        component_offset: u32,
    },
    /// The [`Remove`] event.
    Remove {
        /// The [`ComponentIdx`] of the component to remove.
        component_idx: ComponentIdx,
    },
    /// The event which spawns one queued entity. For internal use only.
    SpawnQueued,
    /// The [`Despawn`] event.
    Despawn,
}

/// Lightweight identifier for an event type.
///
/// Event identifiers are implemented using an [index] and a generation count.
/// The generation count ensures that IDs from removed events are not reused
/// by new events.
///
/// An event identifier is only meaningful in the [`World`] it was created
/// from. Attempting to use an event ID in a different world will have
/// unexpected results.
///
/// [index]: EventIdx
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct EventId {
    index: u32,
    generation: u32,
}

impl EventId {
    /// The event ID which never identifies a live event. This is the default
    /// value for `EventId`.
    pub const NULL: Self = Self {
        index: u32::MAX,
        generation: u32::MAX,
    };

    // ID of the [`SpawnQueued`] event.
    pub(crate) const SPAWN_QUEUED: EventId = Self {
        index: 0,
        generation: 1,
    };

    /// Creates a new event ID from an index and generation count. Returns
    /// `None` if the ID is malformed.
    pub const fn new(index: EventIdx, generation: u32) -> Option<Self> {
        match Key::new(index.as_u32(), generation) {
            Some(k) => Some(Self::from_key(k, index.is_targeted())),
            None => None,
        }
    }

    const fn from_key(k: Key, is_targeted: bool) -> Self {
        Self {
            index: k.index(),
            generation: if is_targeted {
                k.generation().get() & !1
            } else {
                k.generation().get()
            },
        }
    }

    fn as_key(self) -> Key {
        Key::new(self.index, self.generation().get()).unwrap()
    }

    /// Returns whether this ID refers to a [targeted] event.
    ///
    /// [targeted]: Event::IS_TARGETED
    pub const fn is_targeted(&self) -> bool {
        self.generation & 1 == 0
    }

    /// Returns whether this ID refers to an [untargeted] event.
    ///
    /// [untargeted]: Event::IS_TARGETED
    pub const fn is_untargeted(&self) -> bool {
        self.generation & 1 == 1
    }

    /// Returns the index of this ID.
    #[inline]
    pub const fn index(self) -> EventIdx {
        if self.is_targeted() {
            EventIdx::Targeted(TargetedEventIdx(self.index))
        } else {
            EventIdx::Untargeted(UntargetedEventIdx(self.index))
        }
    }

    /// Returns the generation count of this ID.
    pub const fn generation(self) -> NonZeroU32 {
        unsafe { NonZeroU32::new_unchecked(self.generation | 1) }
    }
}

impl Default for EventId {
    fn default() -> Self {
        Self::NULL
    }
}

impl fmt::Debug for EventId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("EventId")
            .field("index", &self.index())
            .field("generation", &self.generation())
            .finish()
    }
}

/// An [`EventId`] with the generation count stripped out.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum EventIdx {
    /// Index to a targeted event.
    Targeted(TargetedEventIdx),
    /// Index to an untargeted event.
    Untargeted(UntargetedEventIdx),
}

impl EventIdx {
    /// Returns the index as a `u32`.
    pub const fn as_u32(self) -> u32 {
        match self {
            EventIdx::Targeted(idx) => idx.0,
            EventIdx::Untargeted(idx) => idx.0,
        }
    }

    /// If this index refers to a targeted event.
    pub const fn is_targeted(self) -> bool {
        matches!(self, Self::Targeted(_))
    }

    /// If this index refers to an untargeted event.
    pub const fn is_untargeted(self) -> bool {
        !self.is_targeted()
    }
}

/// Event index which is known to refer to an untargeted event.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct UntargetedEventIdx(pub u32);

unsafe impl SparseIndex for UntargetedEventIdx {
    const MAX: Self = Self(u32::MAX);

    fn index(self) -> usize {
        self.0.index()
    }

    fn from_index(idx: usize) -> Self {
        Self(u32::from_index(idx))
    }
}

/// Event index which is known to refer to a targeted event.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct TargetedEventIdx(pub u32);

unsafe impl SparseIndex for TargetedEventIdx {
    const MAX: Self = Self(u32::MAX);

    fn index(self) -> usize {
        self.0.index()
    }

    fn from_index(idx: usize) -> Self {
        Self(u32::from_index(idx))
    }
}

pub(crate) struct SpawnQueued;

impl Event for SpawnQueued {
    unsafe fn init(_world: &mut World) -> EventKind {
        EventKind::SpawnQueued
    }
}

/// Metadata for an event.
#[derive(Debug)]
pub struct EventInfo {
    name: Cow<'static, str>,
    id: EventId,
    kind: EventKind,
    type_id: Option<TypeId>,
    layout: Layout,
    drop: DropFn,
    is_immutable: bool,
}

impl EventInfo {
    /// Gets the name of the event.
    ///
    /// This name is intended for debugging purposes and should not be relied
    /// upon for correctness.
    pub fn name(&self) -> &str {
        &self.name
    }

    /// Gets the ID of the event.
    pub fn id(&self) -> EventId {
        self.id
    }

    /// Gets the [`EventKind`] of the event.
    pub fn kind(&self) -> EventKind {
        self.kind
    }

    /// Gets the [`TypeId`] of the event, if any.
    pub fn type_id(&self) -> Option<TypeId> {
        self.type_id
    }

    /// Gets the [`Layout`] of the event.
    pub fn layout(&self) -> Layout {
        self.layout
    }

    /// Gets the [`DropFn`] of the event.
    pub fn drop(&self) -> DropFn {
        self.drop
    }

    /// Gets the [immutability] of the event.
    ///
    /// [immutability]: Event::IS_IMMUTABLE
    pub fn is_immutable(&self) -> bool {
        self.is_immutable
    }
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
    /// If this event is [targeted](Event::IS_TARGETED).
    pub is_targeted: bool,
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
    pub(crate) unsafe fn push_front<E: Event>(&mut self, event: E, idx: u32) {
        let meta = if E::IS_TARGETED {
            EventMeta::Targeted {
                idx: TargetedEventIdx(idx),
                target: event.target(),
            }
        } else {
            EventMeta::Untargeted {
                idx: UntargetedEventIdx(idx),
            }
        };

        let event = NonNull::from(self.bump.alloc(event)).cast::<u8>();
        self.items.push(EventQueueItem { meta, event });
    }

    /// Reverses elements in the range `from..`.
    ///
    /// # Safety
    ///
    /// `from` must be in bounds.
    pub(crate) unsafe fn reverse_from(&mut self, from: usize) {
        self.items.get_debug_checked_mut(from..).reverse();
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

// SAFETY: The bump allocator is only accessed behind an exclusive reference to
// the event queue.
unsafe impl Sync for EventQueue {}

impl UnwindSafe for EventQueue {}
impl RefUnwindSafe for EventQueue {}

#[derive(Clone, Copy, Debug)]
pub(crate) struct EventQueueItem {
    pub(crate) meta: EventMeta,
    /// Type-erased pointer to this event. When null, ownership of the event
    /// has been transferred and no destructor needs to run.
    pub(crate) event: NonNull<u8>,
}

// SAFETY: Events are always Send + Sync.
unsafe impl Send for EventQueueItem {}
unsafe impl Sync for EventQueueItem {}

/// Metadata for an event in the event queue.
#[derive(Clone, Copy, Debug)]
pub(crate) enum EventMeta {
    Untargeted {
        idx: UntargetedEventIdx,
    },
    Targeted {
        idx: TargetedEventIdx,
        target: EntityId,
    },
}

impl EventMeta {
    #[inline]
    pub(crate) const fn event_idx(self) -> EventIdx {
        match self {
            EventMeta::Untargeted { idx } => EventIdx::Untargeted(idx),
            EventMeta::Targeted { idx, .. } => EventIdx::Targeted(idx),
        }
    }
}

/// Type-erased pointer to an event. Passed to systems in [`System::run`].
///
/// [`System::run`]: crate::system::System::run
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

    /// Marks the event as owned. It is then the system's responsibility to drop
    /// the event.
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
pub struct EventMut<'a, E> {
    ptr: EventPtr<'a>,
    _marker: PhantomData<&'a mut E>,
}

impl<'a, E> EventMut<'a, E> {
    fn new(ptr: EventPtr<'a>) -> Self {
        Self {
            ptr,
            _marker: PhantomData,
        }
    }

    /// Takes ownership of the event. Any other systems listening for this event
    /// will not run.
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
    /// world.add_system(|r: ReceiverMut<E>| {
    ///     EventMut::take(r.event); // Took ownership of event.
    /// });
    ///
    /// world.add_system(|_: Receiver<E>| panic!("boom"));
    ///
    /// world.send(E);
    /// // ^ No panic occurs because the first system took
    /// // ownership of the event before the second could run.
    /// ```
    pub fn take(this: Self) -> E {
        let res = unsafe { this.ptr.as_ptr().as_ptr().cast::<E>().read() };
        unsafe { this.ptr.set_owned() };
        res
    }
}

unsafe impl<E: Send> Send for EventMut<'_, E> {}
unsafe impl<E: Sync> Sync for EventMut<'_, E> {}

impl<E> Deref for EventMut<'_, E> {
    type Target = E;

    fn deref(&self) -> &Self::Target {
        unsafe { self.ptr.as_ptr().cast::<E>().as_ref() }
    }
}

impl<E> DerefMut for EventMut<'_, E> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { self.ptr.as_ptr().cast::<E>().as_mut() }
    }
}

impl<E: fmt::Debug> fmt::Debug for EventMut<'_, E> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("EventMut").field(&**self).finish()
    }
}

/// A [`SystemParam`] which listens for events of type `E`.
///
/// For more information, see the relevant [tutorial
/// chapter](crate::tutorial::ch01_systems_and_events#systems-and-events).
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
/// world.add_system(|r: Receiver<E>| {
///     println!("got event of type E!");
/// });
/// ```
#[derive(Clone, Copy)]
pub struct Receiver<'a, E: Event, Q: ReceiverQuery + 'static = NullReceiverQuery> {
    /// A reference to the received event.
    pub event: &'a E,
    /// The result of the query. This field is meaningless if `E` is not a
    /// targeted event.
    pub query: Q::Item<'a>,
}

unsafe impl<E: Event> SystemParam for Receiver<'_, E> {
    type State = ();

    type Item<'a> = Receiver<'a, E>;

    fn init(world: &mut World, config: &mut Config) -> Result<Self::State, InitError> {
        let () = AssertUntargetedEvent::<E>::ASSERTION;

        set_received_event::<E>(world, config, Access::Read)?;

        Ok(())
    }

    unsafe fn get<'a>(
        _state: &'a mut Self::State,
        _info: &'a SystemInfo,
        event_ptr: EventPtr<'a>,
        _world: UnsafeWorldCell<'a>,
    ) -> Self::Item<'a> {
        Receiver {
            // SAFETY:
            // - We have permission to access the event immutably.
            // - System was configured to listen for `E`.
            event: event_ptr.as_ptr().cast().as_ref(),
            query: (),
        }
    }

    fn refresh_archetype(_state: &mut Self::State, _arch: &Archetype) {}

    fn remove_archetype(_state: &mut Self::State, _arch: &Archetype) {}
}

unsafe impl<E: Event, Q: Query + 'static> SystemParam for Receiver<'_, E, Q> {
    type State = FetcherState<Q>;

    type Item<'a> = Receiver<'a, E, Q>;

    fn init(world: &mut World, config: &mut Config) -> Result<Self::State, InitError> {
        let () = AssertTargetedEvent::<E>::ASSERTION;

        set_received_event::<E>(world, config, Access::Read)?;

        let (expr, state) = Q::init(world, config)?;

        let res = FetcherState::new(state);

        config.targeted_event_expr = expr.expr.clone();

        if let Ok(new_component_access) = expr.or(&config.component_access) {
            config.component_access = new_component_access;
        } else {
            return Err(InitError(
                format!(
                    "query `{}` has incompatible component access with previous queries in this \
                     system",
                    any::type_name::<Q>()
                )
                .into(),
            ));
        }

        Ok(res)
    }

    unsafe fn get<'a>(
        state: &'a mut Self::State,
        _info: &'a SystemInfo,
        event_ptr: EventPtr<'a>,
        world: UnsafeWorldCell<'a>,
    ) -> Self::Item<'a> {
        assert!(E::IS_TARGETED);
        let event = event_ptr.as_ptr().cast::<E>().as_ref();
        let target = event.target();

        // SAFETY: The target entity is guaranteed to match the query.
        let query = state
            .get_mut(world.entities(), target)
            .unwrap_debug_checked();

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
    E: Event + fmt::Debug,
    Q: ReceiverQuery,
    Q::Item<'a>: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Receiver")
            .field("event", &self.event)
            .field("query", &self.query)
            .finish()
    }
}

/// Like [`Receiver`], but provides mutable access to the received event.
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

unsafe impl<E: Event> SystemParam for ReceiverMut<'_, E> {
    type State = ();

    type Item<'a> = ReceiverMut<'a, E>;

    fn init(world: &mut World, config: &mut Config) -> Result<Self::State, InitError> {
        let () = AssertMutable::<E>::EVENT;
        let () = AssertUntargetedEvent::<E>::ASSERTION;

        set_received_event::<E>(world, config, Access::ReadWrite)?;

        Ok(())
    }

    unsafe fn get<'a>(
        _state: &'a mut Self::State,
        _info: &'a SystemInfo,
        event_ptr: EventPtr<'a>,
        _world: UnsafeWorldCell<'a>,
    ) -> Self::Item<'a> {
        ReceiverMut {
            event: EventMut::new(event_ptr),
            query: (),
        }
    }

    fn refresh_archetype(_state: &mut Self::State, _arch: &Archetype) {}

    fn remove_archetype(_state: &mut Self::State, _arch: &Archetype) {}
}

unsafe impl<E: Event, Q: Query + 'static> SystemParam for ReceiverMut<'_, E, Q> {
    type State = FetcherState<Q>;

    type Item<'a> = ReceiverMut<'a, E, Q>;

    fn init(world: &mut World, config: &mut Config) -> Result<Self::State, InitError> {
        let () = AssertMutable::<E>::EVENT;
        let () = AssertTargetedEvent::<E>::ASSERTION;

        set_received_event::<E>(world, config, Access::ReadWrite)?;

        let (expr, state) = Q::init(world, config)?;

        let res = FetcherState::new(state);

        config.targeted_event_expr = expr.expr.clone();

        if let Ok(new_component_access) = expr.or(&config.component_access) {
            config.component_access = new_component_access;
        } else {
            return Err(InitError(
                format!(
                    "query `{}` has incompatible component access with previous queries in this \
                     system",
                    any::type_name::<Q>()
                )
                .into(),
            ));
        }

        Ok(res)
    }

    unsafe fn get<'a>(
        state: &'a mut Self::State,
        _info: &'a SystemInfo,
        event_ptr: EventPtr<'a>,
        world: UnsafeWorldCell<'a>,
    ) -> Self::Item<'a> {
        assert!(E::IS_TARGETED);

        let event = EventMut::<E>::new(event_ptr);
        let target = event.target();

        // SAFETY: The target entity is guaranteed to match the query.
        let query = state
            .get_mut(world.entities(), target)
            .unwrap_debug_checked();

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
    E: Event + fmt::Debug,
    Q: ReceiverQuery,
    Q::Item<'a>: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Receiver")
            .field("event", &self.event)
            .field("query", &self.query)
            .finish()
    }
}

fn set_received_event<E: Event>(
    world: &mut World,
    config: &mut Config,
    access: Access,
) -> Result<EventId, InitError> {
    let id = world.add_event::<E>();

    if let Some(received_event) = config.received_event {
        if received_event != id {
            let other = world
                .events()
                .get(received_event)
                .map_or("<unknown>", |info| info.name());

            return Err(InitError(
                format!(
                    "tried to set `{}` as the received event for this system, but the system was \
                     already configured to receive `{other}`. Systems must listen for exactly one \
                     event type",
                    any::type_name::<E>(),
                )
                .into(),
            ));
        }
    }

    config.received_event = Some(id);

    if !config.received_event_access.set_if_compatible(access) {
        return Err(InitError(
            format!(
                "tried to set `{access:?}` as the received event access for this system, but it \
                 was already set to `{:?}`",
                config.received_event_access
            )
            .into(),
        ));
    }

    Ok(id)
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

/// A [`SystemParam`] for sending events from the set `T`.
///
/// For more information, see the relevant [tutorial
/// chapter](crate::tutorial::ch03_sending_events_from_systems).
#[derive(Clone, Copy)]
pub struct Sender<'a, T: EventSet> {
    state: &'a T::EventIndices,
    world: UnsafeWorldCell<'a>,
}

impl<T: EventSet> Sender<'_, T> {
    /// Add an [`Event`] to the queue of events to send. The queue is flushed
    /// once the system returns.
    ///
    /// # Panics
    ///
    /// Panics if `E` is not in the [`EventSet`] of this sender.
    #[track_caller]
    pub fn send<E: Event>(&mut self, event: E) {
        // The event type and event set are all compile time known, so the compiler
        // should be able to optimize this away.
        let event_idx = T::event_idx_of::<E>(self.state).unwrap_or_else(|| {
            panic!(
                "event `{}` is not in the `EventSet` of this `Sender`",
                any::type_name::<E>()
            )
        });

        unsafe { self.world.send_with_index(event, event_idx) }
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
    /// # world.add_system(|_: Receiver<E>, mut sender: Sender<Insert<C>>| {
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
        self.send(Insert::new(entity, component))
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
    /// # world.add_system(move |_: Receiver<E>, mut sender: Sender<Remove<C>>| {
    /// sender.send(Remove::<C>::new(entity));
    /// # });
    /// ```
    ///
    /// # Panics
    ///
    /// Panics if `Remove<C>` is not in the [`EventSet`] of this sender.
    #[track_caller]
    pub fn remove<C: Component>(&mut self, entity: EntityId) {
        self.send(Remove::<C>::new(entity))
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
    /// # world.add_system(move |_: Receiver<E>, mut sender: Sender<Despawn>| {
    /// sender.send(Despawn(entity));
    /// # });
    /// ```
    ///
    /// # Panics
    ///
    /// Panics if `Despawn` is not in the [`EventSet`] of this sender.
    #[track_caller]
    pub fn despawn(&mut self, entity: EntityId) {
        self.send(Despawn(entity))
    }
}

unsafe impl<T: EventSet> SystemParam for Sender<'_, T> {
    type State = T::EventIndices;

    type Item<'a> = Sender<'a, T>;

    fn init(world: &mut World, config: &mut Config) -> Result<Self::State, InitError> {
        if !config
            .event_queue_access
            .set_if_compatible(Access::ReadWrite)
        {
            return Err(InitError(
                format!(
                    "`{}` has conflicting access with a previous system parameter. Only one \
                     system parameter can send events",
                    any::type_name::<Self>()
                )
                .into(),
            ));
        }

        let state = T::new_state(world);

        T::for_each_idx(&state, |idx| {
            match idx {
                EventIdx::Untargeted(i) => config.sent_untargeted_events.insert(i),
                EventIdx::Targeted(i) => config.sent_targeted_events.insert(i),
            };
        });

        Ok(state)
    }

    unsafe fn get<'a>(
        state: &'a mut Self::State,
        _info: &'a SystemInfo,
        _event_ptr: EventPtr<'a>,
        world: UnsafeWorldCell<'a>,
    ) -> Self::Item<'a> {
        Sender { state, world }
    }

    fn refresh_archetype(_state: &mut Self::State, _arch: &Archetype) {}

    fn remove_archetype(_state: &mut Self::State, _arch: &Archetype) {}
}

impl<T: EventSet> fmt::Debug for Sender<'_, T>
where
    T::EventIndices: fmt::Debug,
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
/// being correct.
pub unsafe trait EventSet {
    /// The set of event indices.
    type EventIndices: Send + Sync + 'static;

    /// Creates the set of event indices.
    fn new_state(world: &mut World) -> Self::EventIndices;

    /// Gets an event index from the set. Returns `None` if event `E`
    /// is not in the set.
    fn event_idx_of<E: Event>(state: &Self::EventIndices) -> Option<u32>;

    /// Run a function on each event index in the set.
    fn for_each_idx<F: FnMut(EventIdx)>(state: &Self::EventIndices, f: F);
}

unsafe impl<E: Event> EventSet for E {
    // Either a targeted event index or an untargeted event index.
    type EventIndices = u32;

    fn new_state(world: &mut World) -> Self::EventIndices {
        match world.add_event::<E>().index() {
            EventIdx::Untargeted(idx) => idx.0,
            EventIdx::Targeted(idx) => idx.0,
        }
    }

    #[inline]
    fn event_idx_of<F: Event>(state: &Self::EventIndices) -> Option<u32> {
        (TypeId::of::<F>() == TypeId::of::<E>()).then_some(*state)
    }

    fn for_each_idx<F: FnMut(EventIdx)>(state: &Self::EventIndices, mut f: F) {
        f(if E::IS_TARGETED {
            EventIdx::Targeted(TargetedEventIdx(*state))
        } else {
            EventIdx::Untargeted(UntargetedEventIdx(*state))
        })
    }
}

macro_rules! impl_event_set_tuple {
    ($(($E:ident, $e:ident)),*) => {
        #[allow(clippy::unused_unit)]
        unsafe impl<$($E: EventSet),*> EventSet for ($($E,)*) {
            type EventIndices = ($($E::EventIndices,)*);

            fn new_state(_world: &mut World) -> Self::EventIndices {
                (
                    $(
                        $E::new_state(_world),
                    )*
                )
            }

            #[inline]
            fn event_idx_of<E: Event>(($($e,)*): &Self::EventIndices) -> Option<u32> {
                $(
                    if let Some(id) = $E::event_idx_of::<E>($e) {
                        return Some(id);
                    }
                )*

                None
            }

            fn for_each_idx<F: FnMut(EventIdx)>(($($e,)*): &Self::EventIndices, mut _f: F) {
                $(
                    $E::for_each_idx($e, &mut _f);
                )*
            }
        }
    };
}

all_tuples!(impl_event_set_tuple, 0, 15, E, e);

/// An [`Event`] which adds component `C` on an entity when sent. If the entity
/// already has the component, then the component is replaced.
///
/// Any system which listens for `Insert<C>` will run before the component is
/// inserted. `Insert<C>` has no effect if the target entity does not exist or
/// the event is consumed before it finishes broadcasting.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
#[repr(C)] // Field order is significant!
pub struct Insert<C> {
    /// The entity to insert the component on.
    pub entity: EntityId,
    /// The component to insert.
    pub component: C,
}

impl<C> Insert<C> {
    /// Create a new instance.
    pub const fn new(entity: EntityId, component: C) -> Self {
        Self { entity, component }
    }
}

impl<C: Component> Event for Insert<C> {
    const IS_TARGETED: bool = true;

    fn target(&self) -> EntityId {
        self.entity
    }

    unsafe fn init(world: &mut World) -> EventKind {
        EventKind::Insert {
            component_idx: world.add_component::<C>().index(),
            component_offset: offset_of!(Self, component)
                .try_into()
                .expect("component offset should fit in a `u32`"),
        }
    }
}

/// An [`Event`] which removes component `C` from an entity when sent. The
/// component is dropped and cannot be recovered.
///
/// Any system which listens for `Remove<C>` will run before the component is
/// removed. `Remove<C>` has no effect if the target entity does not exist or
/// the event is consumed before it finishes broadcasting.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
#[repr(transparent)]
pub struct Remove<C> {
    /// The entity to remove the component from.
    pub entity: EntityId,
    _marker: PhantomData<fn() -> C>,
}

impl<C> Remove<C> {
    /// Create a new instance.
    pub const fn new(entity: EntityId) -> Self {
        Self {
            entity,
            _marker: PhantomData,
        }
    }
}

impl<C: Component> Event for Remove<C> {
    const IS_TARGETED: bool = true;

    fn target(&self) -> EntityId {
        self.entity
    }

    unsafe fn init(world: &mut World) -> EventKind {
        EventKind::Remove {
            component_idx: world.add_component::<C>().index(),
        }
    }
}

/// An [`Event`] which signals the creation of an entity. Contains the
/// [`EntityId`] of the new entity.
///
/// The event by itself has no additional effects, and cannot be used to spawn
/// new entities. Use [`World::spawn`] or [`Sender::spawn`] instead.
#[derive(Event, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct Spawn(#[event(target)] pub EntityId);

/// An [`Event`] which removes an entity from the [`World`] when sent. All
/// components of the target entity are dropped.
///
/// Any system which listens for `Despawn` will run before the entity is
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
/// world.add_system(|r: Receiver<Despawn, ()>| {
///     println!("{:?} is about to despawn!", r.event.0);
/// });
///
/// world.send(Despawn(id));
///
/// assert!(!world.entities().contains(id));
/// ```
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct Despawn(pub EntityId);

impl Event for Despawn {
    const IS_TARGETED: bool = true;

    fn target(&self) -> EntityId {
        self.0
    }

    unsafe fn init(_world: &mut World) -> EventKind {
        EventKind::Despawn
    }
}

/// An [`Event`] sent immediately after a new event is added to the world.
///
/// Contains the [`EventId`] of the added event.
#[derive(Event, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct AddEvent(pub EventId);

/// An [`Event`] sent immediately before an event is removed from the world.
///
/// Contains the [`EventId`] of the event to be removed.
#[derive(Event, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct RemoveEvent(pub EventId);

#[cfg(test)]
mod tests {
    use crate::prelude::*;

    #[test]
    fn remove_spawn_queued_event() {
        let mut world = World::new();

        assert!(world.events().contains(EventId::SPAWN_QUEUED));
        assert!(world.remove_event(EventId::SPAWN_QUEUED).is_none());
    }

    #[test]
    fn change_entity_during_broadcast() {
        let mut world = World::new();

        #[derive(Event)]
        struct E(#[event(target)] EntityId);

        #[derive(Component)]
        struct C(String);

        world.add_system(|r: Receiver<E, ()>, mut s: Sender<Remove<C>>| {
            s.remove::<C>(r.event.0);
        });

        world.add_system(|r: Receiver<E, &mut C>| {
            r.query.0.push_str("123");
        });

        let e = world.spawn();
        world.insert(e, C("abc".into()));

        world.send(E(e));
    }

    #[test]
    fn event_order() {
        #[derive(Event)]
        struct A;
        #[derive(Event, Debug)]
        struct B(i32);
        #[derive(Event, Debug)]
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

        world.add_system(get_a_send_b);
        world.add_system(get_b_send_c);
        world.add_system(get_c);

        world.send(A);

        assert_eq!(
            world.get_component::<Result>(res).unwrap().0.as_slice(),
            &[0, 1, 2, 3, 4, 5]
        );
    }
}
