use alloc::borrow::Cow;
use alloc::collections::BTreeMap;
use core::alloc::Layout;
use core::any::TypeId;
use core::marker::PhantomData;
use core::num::NonZeroU32;
use core::ops::{Deref, DerefMut};
use core::ptr::NonNull;
use core::{any, fmt, mem};
use std::collections::btree_map::Entry;
use std::ops::Index;

use bumpalo::Bump;
use evenio_macros::all_tuples;
pub use evenio_macros::Event;
use memoffset::offset_of;

use crate::access::Access;
use crate::archetype::Archetype;
use crate::component::ComponentIdx;
use crate::debug_checked::{GetDebugChecked, UnwrapDebugChecked};
use crate::entity::EntityId;
use crate::fetch::{Fetcher, FetcherState};
use crate::prelude::{Component, ComponentId};
use crate::query::Query;
use crate::slot_map::{Key, SlotMap};
use crate::sparse::SparseIndex;
use crate::system::{Config, InitError, SystemId, SystemInfo, SystemParam};
use crate::world::{UnsafeWorldCell, World};
use crate::DropFn;

#[derive(Debug)]
pub struct Events {
    untargeted_events: SlotMap<EventInfo>,
    targeted_events: SlotMap<EventInfo>,
    by_type_id: BTreeMap<TypeId, EventId>,
}

impl Events {
    pub(crate) fn new() -> Self {
        Self {
            untargeted_events: SlotMap::new(),
            targeted_events: SlotMap::new(),
            by_type_id: BTreeMap::new(),
        }
    }

    pub(crate) fn add(&mut self, desc: EventDescriptor) -> (EventId, bool) {
        let info = EventInfo {
            id: EventId::NULL,
            name: desc.name,
            kind: desc.kind,
            type_id: desc.type_id,
            layout: desc.layout,
            drop: desc.drop,
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

    pub fn get(&self, id: EventId) -> Option<&EventInfo> {
        let k = id.as_key();
        match id.index() {
            EventIdx::Targeted(_) => self.targeted_events.get(k),
            EventIdx::Untargeted(_) => self.untargeted_events.get(k),
        }
    }

    #[inline]
    pub fn get_by_index(&self, idx: EventIdx) -> Option<&EventInfo> {
        match idx {
            EventIdx::Untargeted(idx) => Some(self.untargeted_events.get_by_index(idx.0)?.1),
            EventIdx::Targeted(idx) => Some(self.targeted_events.get_by_index(idx.0)?.1),
        }
    }

    pub fn get_by_type_id(&self, type_id: TypeId) -> Option<&EventInfo> {
        let idx = *self.by_type_id.get(&type_id)?;
        Some(unsafe { self.get(idx).unwrap_debug_checked() })
    }

    pub fn contains(&self, id: EventId) -> bool {
        self.get(id).is_some()
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

impl SystemParam for &'_ Events {
    type State = ();

    type Item<'a> = &'a Events;

    fn init(_world: &mut World, _config: &mut Config) -> Result<Self::State, InitError> {
        Ok(())
    }

    unsafe fn get_param<'a>(
        _state: &'a mut Self::State,
        _system_info: &'a SystemInfo,
        _event_ptr: EventPtr<'a>,
        world: UnsafeWorldCell<'a>,
    ) -> Self::Item<'a> {
        world.events()
    }

    unsafe fn refresh_archetype(_state: &mut Self::State, _arch: &Archetype) {}

    unsafe fn remove_archetype(_state: &mut Self::State, _arch: &Archetype) {}
}

pub trait Event: Send + Sync + 'static {
    const IS_TARGETED: bool;

    const MUTABLE: bool = true;

    fn target(&self) -> EntityId;

    unsafe fn init(world: &mut World) -> EventKind {
        let _ = world;
        EventKind::Other
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default, Debug)]
#[non_exhaustive]
pub enum EventKind {
    /// An event not covered by one of the other variants. Most events are of
    /// this kind.
    #[default]
    Other,
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
    /// The [`Spawn`] event.
    Spawn,
    /// The [`Despawn`] event.
    Despawn,
    /// The [`Call`] event.
    Call {
        /// The [`EventId`] of the event to send.
        event_id: EventId,
        /// Cached offset from the beginning of the event to the
        /// [`Call::event`] field.
        event_offset: u32,
    },
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

    pub const fn new(index: EventIdx, generation: NonZeroU32) -> Option<Self> {
        todo!()
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
        Key::new(self.index, self.generation()).unwrap()
    }

    pub const fn is_targeted(&self) -> bool {
        self.generation & 1 == 0
    }

    pub const fn is_untargeted(&self) -> bool {
        !self.is_targeted()
    }

    #[inline]
    pub const fn index(self) -> EventIdx {
        if self.is_targeted() {
            EventIdx::Targeted(TargetedEventIdx(self.index))
        } else {
            EventIdx::Untargeted(UntargetedEventIdx(self.index))
        }
    }

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

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum EventIdx {
    Targeted(TargetedEventIdx),
    Untargeted(UntargetedEventIdx),
}

impl EventIdx {
    pub const fn as_u32(self) -> u32 {
        match self {
            EventIdx::Targeted(idx) => idx.0,
            EventIdx::Untargeted(idx) => idx.0,
        }
    }
}

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

#[derive(Debug)]
pub struct EventInfo {
    id: EventId,
    name: Cow<'static, str>,
    kind: EventKind,
    type_id: Option<TypeId>,
    layout: Layout,
    drop: DropFn,
}

impl EventInfo {
    pub fn id(&self) -> EventId {
        self.id
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn kind(&self) -> EventKind {
        self.kind
    }

    pub fn type_id(&self) -> Option<TypeId> {
        self.type_id
    }

    pub fn layout(&self) -> Layout {
        self.layout
    }

    pub fn drop(&self) -> DropFn {
        self.drop
    }
}

#[derive(Clone, Debug)]
pub struct EventDescriptor {
    pub name: Cow<'static, str>,
    pub type_id: Option<TypeId>,
    pub is_targeted: bool,
    pub kind: EventKind,
    pub layout: Layout,
    pub drop: DropFn,
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

    pub(crate) unsafe fn get_debug_checked_mut(&mut self, idx: usize) -> &mut EventQueueItem {
        self.items.get_debug_checked_mut(idx)
    }

    #[inline]
    pub(crate) unsafe fn push<E: Event>(&mut self, event: E, idx: u32) {
        let meta = if E::IS_TARGETED {
            EventMeta::Entity {
                idx: TargetedEventIdx(idx),
                target: event.target(),
            }
        } else {
            EventMeta::Global {
                idx: UntargetedEventIdx(idx),
            }
        };

        let event = self.bump.alloc(event) as *mut E as *mut u8;
        self.items.push(EventQueueItem { meta, event });
    }

    pub(crate) fn iter(&self) -> impl Iterator<Item = &EventQueueItem> {
        self.items.iter()
    }

    /// Clears the event queue and resets the internal bump allocator.
    ///
    /// Any remaining event pointers are invalidated.
    pub(crate) fn reset(&mut self) {
        self.items.clear();
        self.bump.reset();
    }

    pub(crate) fn len(&self) -> usize {
        self.items.len()
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
    pub(crate) meta: EventMeta,
    /// Type-erased pointer to this event. When null, ownership of the event
    /// has been transferred and no destructor needs to run.
    pub(crate) event: *mut u8,
}

/// Metadata for an event in the event queue.
#[derive(Clone, Copy, Debug)]
pub(crate) enum EventMeta {
    Global {
        idx: UntargetedEventIdx,
    },
    Entity {
        idx: TargetedEventIdx,
        target: EntityId,
    },
}

impl EventMeta {
    #[inline]
    pub(crate) const fn event_idx(self) -> EventIdx {
        match self {
            EventMeta::Global { idx } => EventIdx::Untargeted(idx),
            EventMeta::Entity { idx, .. } => EventIdx::Targeted(idx),
        }
    }
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
            const ASSERTION: () = assert!(!E::IS_TARGETED, "TODO: error message");
        }

        let _ = AssertGlobalEvent::<E>::ASSERTION;

        set_received_event::<E>(world, config, Access::Read)?;

        Ok(())
    }

    unsafe fn get_param<'a>(
        _state: &'a mut Self::State,
        _system_info: &'a SystemInfo,
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

    unsafe fn refresh_archetype(_state: &mut Self::State, _arch: &Archetype) {}

    unsafe fn remove_archetype(_state: &mut Self::State, _arch: &Archetype) {}
}

impl<E: Event, Q: Query + 'static> SystemParam for Receiver<'_, E, Q> {
    type State = FetcherState<Q>;

    type Item<'a> = Receiver<'a, E, Q>;

    fn init(world: &mut World, config: &mut Config) -> Result<Self::State, InitError> {
        struct AssertEntityEvent<E>(PhantomData<E>);

        impl<E: Event> AssertEntityEvent<E> {
            const ASSERTION: () = assert!(E::IS_TARGETED, "TODO: error message");
        }

        let _ = AssertEntityEvent::<E>::ASSERTION;

        set_received_event::<E>(world, config, Access::Read)?;

        let (expr, state) = Q::init(world, config)?;

        let res = FetcherState::new(state);

        config.entity_event_expr = expr.expr.clone();

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

    unsafe fn get_param<'a>(
        state: &'a mut Self::State,
        system_info: &'a SystemInfo,
        event_ptr: EventPtr<'a>,
        world: UnsafeWorldCell<'a>,
    ) -> Self::Item<'a> {
        let event = event_ptr.as_event::<E>();

        assert!(E::IS_TARGETED);
        let target = event.target();

        // SAFETY: The target entity is guaranteed to match the query.
        let query = state
            .get_mut(world.entities(), target)
            .unwrap_debug_checked();

        Receiver { event, query }
    }

    unsafe fn refresh_archetype(state: &mut Self::State, arch: &Archetype) {
        state.refresh_archetype(arch)
    }

    unsafe fn remove_archetype(state: &mut Self::State, arch: &Archetype) {
        state.remove_archetype(arch)
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
            const ASSERTION: () = assert!(!E::IS_TARGETED, "TODO: error message");
        }

        let _ = AssertGlobalEvent::<E>::ASSERTION;

        set_received_event::<E>(world, config, Access::ReadWrite)?;

        Ok(())
    }

    unsafe fn get_param<'a>(
        state: &'a mut Self::State,
        system_info: &'a SystemInfo,
        event_ptr: EventPtr<'a>,
        world: UnsafeWorldCell<'a>,
    ) -> Self::Item<'a> {
        ReceiverMut {
            event: event_ptr.as_event_mut(),
            query: (),
        }
    }

    unsafe fn refresh_archetype(state: &mut Self::State, arch: &Archetype) {}

    unsafe fn remove_archetype(state: &mut Self::State, arch: &Archetype) {}
}

impl<E: Event, Q: Query + 'static> SystemParam for ReceiverMut<'_, E, Q> {
    type State = FetcherState<Q>;

    type Item<'a> = ReceiverMut<'a, E, Q>;

    fn init(world: &mut World, config: &mut Config) -> Result<Self::State, InitError> {
        struct AssertEntityEvent<E>(PhantomData<E>);

        impl<E: Event> AssertEntityEvent<E> {
            const ASSERTION: () = assert!(E::IS_TARGETED, "TODO: error message");
        }

        let _ = AssertEntityEvent::<E>::ASSERTION;

        set_received_event::<E>(world, config, Access::ReadWrite)?;

        let (expr, state) = Q::init(world, config)?;

        let res = FetcherState::new(state);

        config.entity_event_expr = expr.expr.clone();

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

    unsafe fn get_param<'a>(
        state: &'a mut Self::State,
        system_info: &'a SystemInfo,
        event_ptr: EventPtr<'a>,
        world: UnsafeWorldCell<'a>,
    ) -> Self::Item<'a> {
        let event = event_ptr.as_event_mut::<E>();

        assert!(E::IS_TARGETED);
        let target = event.target();

        // SAFETY: The target entity is guaranteed to match the query.
        let query = state
            .get_mut(world.entities(), target)
            .unwrap_debug_checked();

        ReceiverMut { event, query }
    }

    unsafe fn refresh_archetype(state: &mut Self::State, arch: &Archetype) {
        state.refresh_archetype(arch)
    }

    unsafe fn remove_archetype(state: &mut Self::State, arch: &Archetype) {
        state.refresh_archetype(arch)
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
                .map(|info| info.name())
                .unwrap_or("<unknown>");

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

#[derive(Clone, Copy)]
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

        unsafe { self.world.push_event_with_index(event, event_idx) }
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

    #[track_caller]
    pub fn insert<C: Component>(&mut self, entity: EntityId, component: C) {
        self.send(Insert::new(entity, component))
    }

    #[track_caller]
    pub fn remove<C: Component>(&mut self, entity: EntityId) {
        self.send(Remove::<C>::new(entity))
    }

    #[track_caller]
    pub fn despawn(&mut self, entity: EntityId) {
        self.send(Despawn(entity))
    }

    #[track_caller]
    pub fn call<E: Event>(&mut self, system: SystemId, event: E) {
        self.send(Call::new(system, event))
    }
}

impl<T: EventSet> SystemParam for Sender<'_, T> {
    type State = T::State;

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

        if !config
            .reserve_entity_access
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

        let state = T::new_state(world);

        T::for_each_idx(&state, |idx| {
            match idx {
                EventIdx::Untargeted(i) => config.sent_global_events.insert(i),
                EventIdx::Targeted(i) => config.sent_entity_events.insert(i),
            };
        });

        Ok(state)
    }

    unsafe fn get_param<'a>(
        state: &'a mut Self::State,
        system_info: &'a SystemInfo,
        event_ptr: EventPtr<'a>,
        world: UnsafeWorldCell<'a>,
    ) -> Self::Item<'a> {
        Sender { state, world }
    }

    unsafe fn refresh_archetype(state: &mut Self::State, arch: &Archetype) {}

    unsafe fn remove_archetype(state: &mut Self::State, arch: &Archetype) {}
}

impl<T: EventSet> fmt::Debug for Sender<'_, T>
where
    T::State: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Sender")
            .field("state", &self.state)
            .field("world", &self.world)
            .finish()
    }
}

pub unsafe trait EventSet {
    type State: Send + Sync + 'static;

    fn new_state(world: &mut World) -> Self::State;

    fn event_idx_of<E: Event>(state: &Self::State) -> Option<u32>;

    fn for_each_idx<F: FnMut(EventIdx)>(state: &Self::State, f: F);
}

unsafe impl<E: Event> EventSet for E {
    // Either an entity event index or global event index.
    type State = u32;

    fn new_state(world: &mut World) -> Self::State {
        match world.add_event::<E>().index() {
            EventIdx::Untargeted(idx) => idx.0,
            EventIdx::Targeted(idx) => idx.0,
        }
    }

    #[inline]
    fn event_idx_of<F: Event>(state: &Self::State) -> Option<u32> {
        (TypeId::of::<F>() == TypeId::of::<E>()).then_some(*state)
    }

    fn for_each_idx<F: FnMut(EventIdx)>(state: &Self::State, mut f: F) {
        f(if E::IS_TARGETED {
            EventIdx::Targeted(TargetedEventIdx(*state))
        } else {
            EventIdx::Untargeted(UntargetedEventIdx(*state))
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
            fn event_idx_of<E: Event>(($($e,)*): &Self::State) -> Option<u32> {
                $(
                    if let Some(id) = $E::event_idx_of::<E>($e) {
                        return Some(id);
                    }
                )*

                None
            }

            fn for_each_idx<F: FnMut(EventIdx)>(($($e,)*): &Self::State, mut _f: F) {
                $(
                    $E::for_each_idx($e, &mut _f);
                )*
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

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
#[repr(transparent)]
pub struct Remove<C> {
    pub entity: EntityId,
    _marker: PhantomData<fn() -> C>,
}

impl<C> Remove<C> {
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

/// An [`Event`] which signals the creation of an entity.
///
/// Note that the contained [`EntityId`] may or may not refer to a live entity
/// at the time the event is sent.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct Spawn(pub EntityId);

impl Event for Spawn {
    const IS_TARGETED: bool = false;

    const MUTABLE: bool = false;

    fn target(&self) -> EntityId {
        unimplemented!("`Spawn` does not have a target entity")
    }

    unsafe fn init(_world: &mut World) -> EventKind {
        EventKind::Spawn
    }
}

/// An [`Event`] which removes an entity from the [`World`].
///
/// `Despawn` has no effect if the target entity does not exist or the event is
/// consumed before it finishes broadcasting. All components of the target
/// entity are dropped.
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

/// An [`Event`] which sends an event to a specific system.
///
/// `Call` allows users to bypass the usual event broadcasting behavior in order
/// to send an event to exactly one system, identified by a [`SystemId`] value.
/// Here are a few scenarios where this could be useful:
/// - A function-like system needs to "return" a value to a "caller".
/// - An event sender has ahead-of-time knowledge about which systems are
///   interested in the event, and would like to choose which systems receive
///   the event for performance reasons.
/// - Interacting with code outside the user's control in a hacky way.
///
/// Note that `Call<E>` is itself an event which can be listened for and
/// consumed as usual. The inner event `E` is only sent once `Call<E>` has
/// finished broadcasting without being consumed.
///
/// # Examples
///
/// ```
/// use evenio::prelude::*;
///
/// let mut world = World::new();
///
/// #[derive(Event)]
/// struct Foo;
///
/// fn system_1(_: Receiver<Foo>) {
///     println!("OK");
/// }
/// fn system_2(_: Receiver<Foo>) {
///     panic!("not OK");
/// }
///
/// let system_1_id = world.add_system(system_1);
/// world.add_system(system_2);
///
/// // Although both `system_1` and `system_2` listen for `Foo`, only `system_1`
/// // will receive this event.
/// world.send(Call {
///     system: system_1_id,
///     event: Foo,
/// });
/// ```
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
#[repr(C)] // Field order is significant!
pub struct Call<E> {
    /// Identifier of the system that will receive the event. If the ID is
    /// invalid, then no system will receive the event.
    pub system: SystemId,
    /// The event to send to the system.
    pub event: E,
}

impl<E> Call<E> {
    pub const fn new(system: SystemId, event: E) -> Self {
        Self { system, event }
    }
}

impl<E: Event> Event for Call<E> {
    const IS_TARGETED: bool = false;

    fn target(&self) -> EntityId {
        unimplemented!()
    }

    unsafe fn init(world: &mut World) -> EventKind {
        EventKind::Call {
            event_id: world.add_event::<E>(),
            event_offset: offset_of!(Self, event)
                .try_into()
                .expect("event offset should fit in a `u32`"),
        }
    }
}

#[derive(Event, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct AddEvent(pub EventId);

#[derive(Event, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct RemoveEvent(pub EventId);
