use alloc::borrow::Cow;
use alloc::collections::BTreeMap;
use core::alloc::Layout;
use core::any::TypeId;
use core::marker::PhantomData;
use core::num::NonZeroU32;
use core::ops::{Deref, DerefMut};
use core::ptr::NonNull;
use core::{any, fmt, mem};

use bumpalo::Bump;
use evenio_macros::all_tuples;
use memoffset::offset_of;

use crate::access::Access;
use crate::bit_set::SparseSetIndex;
use crate::component::ComponentIdx;
use crate::debug_checked::UnwrapDebugChecked;
use crate::entity::EntityId;
use crate::fetch::{Fetcher, FetcherState};
use crate::prelude::Component;
use crate::query::Query;
use crate::slot_map::{Key, SlotMap};
use crate::system::{Config, InitError, SystemParam};
use crate::world::{UnsafeWorldCell, World};

#[derive(Debug)]
pub struct Events {
    global_events: SlotMap<EventInfo>,
    entity_events: SlotMap<EventInfo>,
    by_type_id: BTreeMap<TypeId, EventIdx>,
}

impl Events {
    pub(crate) fn new() -> Self {
        Self {
            global_events: SlotMap::new(),
            entity_events: SlotMap::new(),
            by_type_id: BTreeMap::new(),
        }
    }

    pub fn get(&self, id: EventId) -> Option<&EventInfo> {
        match id.to_enum() {
            EventIdEnum::Global(id) => self.global_events.get(id.0),
            EventIdEnum::Entity(id) => self.entity_events.get(id.0),
        }
    }

    #[inline]
    pub fn by_index(&self, idx: EventIdx) -> Option<&EventInfo> {
        match idx {
            EventIdx::Global(idx) => {
                let k = self.global_events.key_at_index(idx.0)?;
                Some(unsafe { self.global_events.get(k).unwrap_debug_checked() })
            }
            EventIdx::Entity(idx) => {
                let k = self.entity_events.key_at_index(idx.0)?;
                Some(unsafe { self.entity_events.get(k).unwrap_debug_checked() })
            }
        }
    }

    pub fn by_type_id(&self, type_id: TypeId) -> Option<&EventInfo> {
        let idx = *self.by_type_id.get(&type_id)?;
        Some(unsafe { self.by_index(idx).unwrap_debug_checked() })
    }
}

pub unsafe trait Event: Send + Sync + 'static {
    const TARGET_OFFSET: Option<usize> = None;

    // TODO: const MUTABLE: bool = true

    fn init(world: &mut World) -> EventKind {
        let _ = world;
        EventKind::Other
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default, Debug)]
#[non_exhaustive]
pub enum EventKind {
    /// An event not covered by one of the other variants.
    #[default]
    Other,
    /// The [`Insert`] event.
    Insert {
        /// The [`ComponentIdx`] of the component to insert.
        component_idx: ComponentIdx,
        /// Cached offset from the beginning of the event to the
        /// [`Insert::component`] field.
        component_offset: usize,
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

/// Performs sanity checks on [`Entity::TARGET_OFFSET`]. Returns true if `E`'s
/// `TARGET_OFFSET` might be correct, and false if it's definitely incorrect.
const fn check_target_offset<E: Event>() -> bool {
    match E::TARGET_OFFSET {
        Some(target_offset) => {
            mem::size_of::<E>() >= mem::size_of::<EventId>()
                && target_offset <= mem::size_of::<E>() - mem::size_of::<EventId>()
        }
        None => true,
    }
}

#[inline]
const fn get_target<E: Event>(event: &E) -> Option<EntityId> {
    match E::TARGET_OFFSET {
        Some(offset) => Some(unsafe {
            *(event as *const E as *const u8)
                .add(offset)
                .cast::<EntityId>()
        }),
        None => None,
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct EventId {
    index: u32,
    generation: u32,
}

impl EventId {
    pub const NULL: Self = Self {
        index: u32::MAX,
        generation: u32::MAX,
    };

    pub fn to_enum(self) -> EventIdEnum {
        if self.generation & 1 == 0 {
            // SAFETY: Generation is nonzero.
            let gen = unsafe { NonZeroU32::new(self.generation | 1).unwrap_debug_checked() };

            // SAFETY: Generation is odd.
            let k = unsafe { Key::new(self.index, gen).unwrap_debug_checked() };

            EventIdEnum::Global(GlobalEventId(k))
        } else {
            // SAFETY: Generation is nonzero.
            let gen = unsafe { NonZeroU32::new(self.generation).unwrap_debug_checked() };

            // SAFETY: Generation is odd.
            let k = unsafe { Key::new(self.index, gen).unwrap_debug_checked() };

            EventIdEnum::Entity(EntityEventId(k))
        }
    }

    pub const fn from_enum(id: EventIdEnum) -> Self {
        match id {
            EventIdEnum::Global(id) => Self {
                index: id.index().0,
                generation: id.0.generation().get(),
            },
            EventIdEnum::Entity(id) => Self {
                index: id.index().0,
                generation: id.0.generation().get() & !1,
            },
        }
    }

    pub fn index(self) -> EventIdx {
        self.to_enum().index()
    }
}

impl Default for EventId {
    fn default() -> Self {
        Self::NULL
    }
}

impl fmt::Debug for EventId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.to_enum().fmt(f)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum EventIdEnum {
    Global(GlobalEventId),
    Entity(EntityEventId),
}

impl EventIdEnum {
    pub const fn index(self) -> EventIdx {
        match self {
            Self::Global(id) => EventIdx::Global(id.index()),
            Self::Entity(id) => EventIdx::Entity(id.index()),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum EventIdx {
    Global(GlobalEventIdx),
    Entity(EntityEventIdx),
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default, Debug)]
pub struct GlobalEventId(Key);

impl GlobalEventId {
    pub const NULL: Self = Self(Key::NULL);

    pub const fn index(self) -> GlobalEventIdx {
        GlobalEventIdx(self.0.index())
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct GlobalEventIdx(pub u32);

impl SparseSetIndex for GlobalEventIdx {
    fn index(self) -> usize {
        self.0 as usize
    }

    fn from_index(idx: usize) -> Self {
        Self(idx as u32)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default, Debug)]
pub struct EntityEventId(Key);

impl EntityEventId {
    pub const NULL: Self = Self(Key::NULL);

    pub const fn index(self) -> EntityEventIdx {
        EntityEventIdx(self.0.index())
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct EntityEventIdx(pub u32);

impl SparseSetIndex for EntityEventIdx {
    fn index(self) -> usize {
        self.0 as usize
    }

    fn from_index(idx: usize) -> Self {
        Self(idx as u32)
    }
}

#[derive(Debug)]
pub struct EventInfo {
    id: EventId,
    name: Cow<'static, str>,
    type_id: Option<TypeId>,
    layout: Layout,
    drop: Option<unsafe fn(NonNull<u8>)>,
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

    pub(crate) unsafe fn push<E: Event>(&mut self, event: E, event_idx: EventIdx) {
        let event_ptr = NonNull::from(self.bump.alloc(event)).cast();

        self.items.push(EventQueueItem {
            event_idx,
            event_ptr: Some(event_ptr),
        })
    }

    /// Clears the event queue and resets the internal bump allocator.
    ///
    /// Any remaining event pointers are invalidated.
    pub(crate) fn reset(&mut self) {
        self.items.clear();
        self.bump.reset();
    }

    pub(crate) unsafe fn set_len(&mut self, new_len: usize) {
        self.items.set_len(new_len)
    }
}

// SAFETY: The bump allocator is only accessed behind an exclusive reference to
// the event queue.
unsafe impl Sync for EventQueue {}

#[derive(Clone, Copy, Debug)]
pub(crate) struct EventQueueItem {
    pub(crate) event_idx: EventIdx,
    /// Type-erased pointer to this event. When `None`, ownership of the event
    /// has been transferred and no destructor needs to run.
    pub(crate) event_ptr: Option<NonNull<u8>>,
}

#[derive(Clone, Copy, Debug)]
pub struct EventPtr<'a> {
    ptr: NonNull<*mut u8>,
    _marker: PhantomData<&'a mut u8>,
}

impl<'a> EventPtr<'a> {
    pub(crate) fn new(ptr: NonNull<*mut u8>) -> Self {
        Self {
            ptr: NonNull::from(ptr),
            _marker: PhantomData,
        }
    }

    pub unsafe fn as_event<E: Event>(self) -> &'a E {
        &*self.ptr.as_ptr().read().cast::<E>()
    }

    pub unsafe fn as_event_mut<E: Event>(self) -> EventMut<'a, E> {
        EventMut {
            ptr: unsafe { &mut *(self.ptr.as_ptr() as *mut *mut E) },
            _marker: PhantomData,
        }
    }

    pub unsafe fn as_ptr(self) -> &'a *const u8 {
        &*(self.ptr.as_ptr() as *const *const u8)
    }

    pub unsafe fn as_ptr_mut(self) -> &'a mut *mut u8 {
        &mut *self.ptr.as_ptr()
    }
}

pub struct EventMut<'a, E> {
    ptr: &'a mut *mut E,
    _marker: PhantomData<&'a mut E>,
}

impl<E> EventMut<'_, E> {
    pub fn take(this: Self) -> E {
        let res = unsafe { this.ptr.read() };

        *this.ptr = core::ptr::null_mut();

        res
    }
}

impl<E> Deref for EventMut<'_, E> {
    type Target = E;

    fn deref(&self) -> &Self::Target {
        unsafe { &**self.ptr }
    }
}

impl<E> DerefMut for EventMut<'_, E> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { &mut **self.ptr }
    }
}

impl<E: fmt::Debug> fmt::Debug for EventMut<'_, E> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("EventMut").field(self.deref()).finish()
    }
}

#[derive(Clone, Copy, Debug)]
pub struct Receiver<'a, E: Event, Q: ReceiverQuery = NullReceiverQuery> {
    pub event: &'a E,
    pub query: Q::Item<'a>,
}

impl<E: Event> SystemParam for Receiver<'_, E> {
    type State = ();

    type Item<'a> = Receiver<'a, E>;

    fn init(world: &mut World, config: &mut Config) -> Result<Self::State, InitError> {
        struct AssertGlobalEvent<E>(PhantomData<E>);

        impl<E: Event> AssertGlobalEvent<E> {
            const ASSERTION: () = assert!(E::TARGET_OFFSET.is_none(), "TODO: error message");
        }

        let _ = AssertGlobalEvent::<E>::ASSERTION;

        let id = world.init_event::<E>();

        if let Some(received_event) = config.received_event {
            if received_event != id {
                return Err(InitError(
                    format!(
                        "`{}` conflicts with a previous system parameter; systems must listen for \
                         exactly one event type",
                        any::type_name::<Self>()
                    )
                    .into(),
                ));
            }
        }

        config.received_event = Some(id);

        if !config.access.received_event.set_if_compatible(Access::Read) {
            return Err(InitError(
                format!(
                    "`{}` has conflicting event access with a previous system parameter",
                    any::type_name::<Self>()
                )
                .into(),
            ));
        }

        Ok(())
    }

    unsafe fn get_param<'a>(
        _state: &'a mut Self::State,
        _system_info: &'a crate::system::SystemInfo,
        event_ptr: EventPtr<'a>,
        _world: UnsafeWorldCell<'a>,
    ) -> Self::Item<'a> {
        Receiver {
            // SAFETY:
            // - We have permission to access the event immutably.
            // - Pointer is nonnull.
            event: event_ptr.as_event(),
            query: (),
        }
    }
}

impl<E: Event, Q: Query + 'static> SystemParam for Receiver<'_, E, Q> {
    type State = FetcherState<Q>;

    type Item<'a> = Receiver<'a, E, Q>;

    fn init(world: &mut World, config: &mut Config) -> Result<Self::State, InitError> {
        struct AssertEntityEvent<E>(PhantomData<E>);

        impl<E: Event> AssertEntityEvent<E> {
            const ASSERTION: () = match E::TARGET_OFFSET {
                Some(n) => assert!(
                    check_target_offset::<E>(),
                    "incorrect `TARGET_OFFSET` value for event type"
                ),
                None => panic!("TODO: error message"),
            };
        }

        let _ = AssertEntityEvent::<E>::ASSERTION;

        todo!();

        Fetcher::init(world, config)
    }

    unsafe fn get_param<'a>(
        state: &'a mut Self::State,
        system_info: &'a crate::system::SystemInfo,
        event_ptr: EventPtr<'a>,
        world: UnsafeWorldCell<'a>,
    ) -> Self::Item<'a> {
        let event = event_ptr.as_event::<E>();
        let target = get_target(event).unwrap();

        let mut fetcher = Fetcher::new(state, world);

        // SAFETY: The target entity is guaranteed to match the query.
        let query = fetcher.temp(target).unwrap_debug_checked();

        Receiver { event, query }
    }

    unsafe fn refresh_archetype(
        state: &mut Self::State,
        reason: crate::system::RefreshArchetypeReason,
        idx: crate::archetype::ArchetypeIdx,
        arch: &crate::archetype::Archetype,
    ) -> bool {
        Fetcher::refresh_archetype(state, reason, idx, arch)
    }
}

pub struct ReceiverMut<'a, E: Event, Q: ReceiverQuery = NullReceiverQuery> {
    pub event: EventMut<'a, E>,
    pub query: Q::Item<'a>,
}

impl<E: Event> SystemParam for ReceiverMut<'_, E> {
    type State = ();

    type Item<'a> = ReceiverMut<'a, E>;

    fn init(world: &mut World, config: &mut Config) -> Result<Self::State, InitError> {
        struct AssertGlobalEvent<E>(PhantomData<E>);

        impl<E: Event> AssertGlobalEvent<E> {
            const ASSERTION: () = assert!(E::TARGET_OFFSET.is_none(), "TODO: error message");
        }

        let _ = AssertGlobalEvent::<E>::ASSERTION;

        let id = world.init_event::<E>();

        if let Some(received_event) = config.received_event {
            if received_event != id {
                return Err(InitError(
                    format!(
                        "`{}` conflicts with a previous system parameter; systems must listen for \
                         exactly one event type",
                        any::type_name::<Self>()
                    )
                    .into(),
                ));
            }
        }

        config.received_event = Some(id);

        if !config
            .access
            .received_event
            .set_if_compatible(Access::ReadWrite)
        {
            return Err(InitError(
                format!(
                    "`{}` has conflicting event access with a previous system parameter",
                    any::type_name::<Self>()
                )
                .into(),
            ));
        }

        Ok(())
    }

    unsafe fn get_param<'a>(
        state: &'a mut Self::State,
        system_info: &'a crate::system::SystemInfo,
        event_ptr: EventPtr<'a>,
        world: UnsafeWorldCell<'a>,
    ) -> Self::Item<'a> {
        ReceiverMut {
            event: event_ptr.as_event_mut(),
            query: (),
        }
    }
}

impl<E: Event, Q: Query + 'static> SystemParam for ReceiverMut<'_, E, Q> {
    type State = FetcherState<Q>;

    type Item<'a> = ReceiverMut<'a, E, Q>;

    fn init(world: &mut World, config: &mut Config) -> Result<Self::State, InitError> {
        struct AssertEntityEvent<E>(PhantomData<E>);

        impl<E: Event> AssertEntityEvent<E> {
            const ASSERTION: () = match E::TARGET_OFFSET {
                Some(n) => assert!(
                    check_target_offset::<E>(),
                    "incorrect `TARGET_OFFSET` value for event type"
                ),
                None => panic!("TODO: error message"),
            };
        }

        let _ = AssertEntityEvent::<E>::ASSERTION;

        Fetcher::init(world, config)
    }

    unsafe fn get_param<'a>(
        state: &'a mut Self::State,
        system_info: &'a crate::system::SystemInfo,
        event_ptr: EventPtr<'a>,
        world: UnsafeWorldCell<'a>,
    ) -> Self::Item<'a> {
        let event = event_ptr.as_event_mut::<E>();
        let target = get_target(&*event).unwrap();

        let mut fetcher = Fetcher::new(state, world);

        // SAFETY: The target entity is guaranteed to match the query.
        let query = fetcher.temp(target).unwrap_debug_checked();

        ReceiverMut { event, query }
    }

    unsafe fn refresh_archetype(
        state: &mut Self::State,
        reason: crate::system::RefreshArchetypeReason,
        idx: crate::archetype::ArchetypeIdx,
        arch: &crate::archetype::Archetype,
    ) -> bool {
        Fetcher::refresh_archetype(state, reason, idx, arch)
    }
}

#[derive(Clone, Copy, Debug)]
pub struct NullReceiverQuery;

pub trait ReceiverQuery: private::Sealed {
    type Item<'a>;
}

impl ReceiverQuery for NullReceiverQuery {
    type Item<'a> = ();
}

impl<Q: Query + 'static> ReceiverQuery for Q {
    type Item<'a> = Q::Item<'a>;
}

mod private {
    use super::*;

    pub trait Sealed {}

    impl Sealed for NullReceiverQuery {}

    impl<Q: Query> Sealed for Q {}
}

#[derive(Clone, Copy, Debug)]
pub struct Sender<'a, T: EventSet> {
    state: &'a T::State,
    world: UnsafeWorldCell<'a>,
}

impl<T: EventSet> Sender<'_, T> {
    /// # Panics
    ///
    /// Panics if the given event type `E` is not in the [`EventSet`] of this
    /// sender. This may become a compile time error in the future.
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

        unsafe { self.world.send_unchecked(event, event_idx) }
    }

    /// # Panics
    ///
    /// Panics if the [`EventSet`] of this sender does not contain [`Spawn`].
    /// This may become a compile time error in the future.
    #[track_caller]
    pub fn spawn(&mut self) -> EntityId {
        let id = unsafe { self.world.reserve_entity() };
        self.send(Spawn(id));
        id
    }
}

impl<T: EventSet> SystemParam for Sender<'_, T> {
    type State = T::State;

    type Item<'a> = Sender<'a, T>;

    fn init(world: &mut World, config: &mut Config) -> Result<Self::State, InitError> {
        if !config
            .access
            .event_queue
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

        if !config
            .access
            .reserve_entity
            .set_if_compatible(Access::ReadWrite)
        {
            return Err(InitError(
                format!(
                    "`{}` has conflicting access with a previous system parameter. Only one \
                     system parameter can reserve entities",
                    any::type_name::<Self>()
                )
                .into(),
            ));
        }

        Ok(T::new_state(world))
    }

    unsafe fn get_param<'a>(
        state: &'a mut Self::State,
        system_info: &'a crate::system::SystemInfo,
        event_ptr: EventPtr<'a>,
        world: UnsafeWorldCell<'a>,
    ) -> Self::Item<'a> {
        Sender { state, world }
    }
}

pub unsafe trait EventSet {
    type State: Send + Sync + 'static;

    fn new_state(world: &mut World) -> Self::State;

    fn event_idx_of<E: Event>(state: &Self::State) -> Option<EventIdx>;
}

unsafe impl<E: Event> EventSet for E {
    type State = u32;

    fn new_state(world: &mut World) -> Self::State {
        match world.init_event::<E>().index() {
            EventIdx::Global(idx) => idx.0,
            EventIdx::Entity(idx) => idx.0,
        }
    }

    #[inline]
    fn event_idx_of<F: Event>(state: &Self::State) -> Option<EventIdx> {
        (TypeId::of::<F>() == TypeId::of::<E>()).then(|| {
            if E::TARGET_OFFSET.is_some() {
                EventIdx::Entity(EntityEventIdx(*state))
            } else {
                EventIdx::Global(GlobalEventIdx(*state))
            }
        })
    }
}

macro_rules! impl_event_set_tuple {
    ($(($E:ident, $e:ident)),*) => {
        unsafe impl<$($E: EventSet),*> EventSet for ($($E,)*) {
            type State = ($($E::State,)*);

            fn new_state(_world: &mut World) -> Self::State {
                (
                    $(
                        $E::new_state(_world),
                    )*
                )
            }

            #[inline]
            fn event_idx_of<E: Event>(($($e,)*): &Self::State) -> Option<EventIdx> {
                $(
                    if let Some(id) = $E::event_idx_of::<E>($e) {
                        return Some(id);
                    }
                )*

                None
            }
        }
    };
}

all_tuples!(impl_event_set_tuple, 0, 15, E, e);

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
#[repr(C)] // Field order is significant!
pub struct Insert<C> {
    pub entity: EntityId,
    pub component: C,
}

impl<C> Insert<C> {
    pub const fn new(entity: EntityId, component: C) -> Self {
        Self { entity, component }
    }
}

unsafe impl<C: Component> Event for Insert<C> {
    const TARGET_OFFSET: Option<usize> = Some(0);

    fn init(world: &mut World) -> EventKind {
        EventKind::Insert {
            component_idx: world.init_component::<C>().index(),
            component_offset: offset_of!(Self, component),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
#[repr(transparent)]
pub struct Remove<C> {
    pub entity: EntityId,
    _marker: PhantomData<fn(C)>,
}

impl<C> Remove<C> {
    pub const fn new(entity: EntityId) -> Self {
        Self {
            entity,
            _marker: PhantomData,
        }
    }
}

unsafe impl<C: Component> Event for Remove<C> {
    const TARGET_OFFSET: Option<usize> = Some(0);

    fn init(world: &mut World) -> EventKind {
        EventKind::Remove {
            component_idx: world.init_component::<C>().index(),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct Spawn(pub EntityId);

unsafe impl Event for Spawn {
    const TARGET_OFFSET: Option<usize> = Some(offset_of!(Self, 0));

    fn init(world: &mut World) -> EventKind {
        EventKind::Spawn
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct Despawn(pub EntityId);

unsafe impl Event for Despawn {
    const TARGET_OFFSET: Option<usize> = Some(offset_of!(Self, 0));

    fn init(world: &mut World) -> EventKind {
        EventKind::Despawn
    }
}

// TODO: `Call<E>` event?
