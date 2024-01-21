use alloc::borrow::Cow;
use alloc::collections::btree_map::Entry;
use alloc::collections::BTreeMap;
use alloc::vec::Vec;
use alloc::{format, vec};
use core::alloc::Layout;
use core::any::TypeId;
use core::marker::PhantomData;
use core::num::NonZeroU32;
use core::ops::{Deref, DerefMut, Index};
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
use crate::prelude::Component;
use crate::query::Query;
use crate::slot_map::{Key, SlotMap};
use crate::sparse::SparseIndex;
use crate::system::{Config, InitError, SystemInfo, SystemParam};
use crate::world::{UnsafeWorldCell, World};

#[derive(Debug)]
pub struct Events {
    untargeted_events: SlotMap<EventInfo>,
    targeted_events: SlotMap<EventInfo>,
    by_type_id: BTreeMap<TypeId, EventId>,
}

impl Events {
    pub(crate) fn new() -> Self {
        let mut this = Self {
            untargeted_events: SlotMap::new(),
            targeted_events: SlotMap::new(),
            by_type_id: BTreeMap::new(),
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
        debug_assert_ne!(type_id, TypeId::of::<SpawnQueued>());

        let idx = *self.by_type_id.get(&type_id)?;
        Some(unsafe { self.get(idx).unwrap_debug_checked() })
    }

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
        _info: &'a SystemInfo,
        _event_ptr: EventPtr<'a>,
        world: UnsafeWorldCell<'a>,
    ) -> Self::Item<'a> {
        world.events()
    }

    unsafe fn refresh_archetype(_state: &mut Self::State, _arch: &Archetype) {}

    unsafe fn remove_archetype(_state: &mut Self::State, _arch: &Archetype) {}
}

/// # Deriving
///
/// ```
/// use evenio::prelude::*;
///
/// #[derive(Event)]
/// #[event(immutable)] // Overrides the default mutability.
/// struct MyEvent {
///     #[event(target)] // Sets the entity returned by `target()`. If absent, the event is untargeted.
///     entity: EntityId,
/// }
///
/// // Also works on tuple structs, enums, and unions. However, `#[event(target)]` is unavailable for non-struct types.
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
/// ```
pub trait Event: Send + Sync + 'static {
    const IS_TARGETED: bool = false;

    const IS_IMMUTABLE: bool = false;

    fn target(&self) -> EntityId {
        unimplemented!()
    }

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
    SpawnQueued,
    /// The [`Despawn`] event.
    Despawn,
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

    // ID of the [`SpawnQueued`] event.
    pub(crate) const SPAWN_QUEUED: EventId = Self {
        index: 0,
        generation: 1,
    };

    pub const fn new(index: EventIdx, generation: NonZeroU32) -> Option<Self> {
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
        Key::new(self.index, self.generation()).unwrap()
    }

    pub const fn is_targeted(&self) -> bool {
        self.generation & 1 == 0
    }

    pub const fn is_untargeted(&self) -> bool {
        self.generation & 1 == 1
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

    pub const fn is_targeted(self) -> bool {
        matches!(self, Self::Targeted(_))
    }

    pub const fn is_untargeted(self) -> bool {
        !self.is_targeted()
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

pub(crate) struct SpawnQueued;

impl Event for SpawnQueued {
    unsafe fn init(_world: &mut World) -> EventKind {
        EventKind::SpawnQueued
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
    is_immutable: bool,
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

    pub fn is_immutable(&self) -> bool {
        self.is_immutable
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

    pub(crate) unsafe fn get_debug_checked_mut(&mut self, idx: usize) -> &mut EventQueueItem {
        self.items.get_debug_checked_mut(idx)
    }

    #[inline]
    pub(crate) unsafe fn push<E: Event>(&mut self, event: E, idx: u32) {
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

        let event = self.bump.alloc(event) as *mut E as *mut u8;
        self.items.push(EventQueueItem { meta, event });
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

#[derive(Clone, Copy, Debug)]
pub struct EventPtr<'a> {
    ptr: NonNull<*mut u8>,
    _marker: PhantomData<&'a mut u8>,
}

impl<'a> EventPtr<'a> {
    pub(crate) fn new(ptr: NonNull<*mut u8>) -> Self {
        Self {
            ptr,
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
        f.debug_tuple("EventMut").field(&**self).finish()
    }
}

#[derive(Clone, Copy)]
pub struct Receiver<'a, E: Event, Q: ReceiverQuery + 'static = NullReceiverQuery> {
    pub event: &'a E,
    pub query: Q::Item<'a>,
}

impl<E: Event> SystemParam for Receiver<'_, E> {
    type State = ();

    type Item<'a> = Receiver<'a, E>;

    fn init(world: &mut World, config: &mut Config) -> Result<Self::State, InitError> {
        let () = AssertUntargetedEvent::<E>::ASSERTION;

        set_received_event::<E>(world, config, Access::Read)?;

        Ok(())
    }

    unsafe fn get_param<'a>(
        _state: &'a mut Self::State,
        _info: &'a SystemInfo,
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

    unsafe fn get_param<'a>(
        state: &'a mut Self::State,
        _info: &'a SystemInfo,
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

pub struct ReceiverMut<'a, E: Event, Q: ReceiverQuery + 'static = NullReceiverQuery> {
    pub event: EventMut<'a, E>,
    pub query: Q::Item<'a>,
}

impl<E: Event> SystemParam for ReceiverMut<'_, E> {
    type State = ();

    type Item<'a> = ReceiverMut<'a, E>;

    fn init(world: &mut World, config: &mut Config) -> Result<Self::State, InitError> {
        let () = AssertMutable::<E>::EVENT;
        let () = AssertUntargetedEvent::<E>::ASSERTION;

        set_received_event::<E>(world, config, Access::ReadWrite)?;

        Ok(())
    }

    unsafe fn get_param<'a>(
        _state: &'a mut Self::State,
        _info: &'a SystemInfo,
        event_ptr: EventPtr<'a>,
        _world: UnsafeWorldCell<'a>,
    ) -> Self::Item<'a> {
        ReceiverMut {
            event: event_ptr.as_event_mut(),
            query: (),
        }
    }

    unsafe fn refresh_archetype(_state: &mut Self::State, _arch: &Archetype) {}

    unsafe fn remove_archetype(_state: &mut Self::State, _arch: &Archetype) {}
}

impl<E: Event, Q: Query + 'static> SystemParam for ReceiverMut<'_, E, Q> {
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

    unsafe fn get_param<'a>(
        state: &'a mut Self::State,
        _info: &'a SystemInfo,
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

        unsafe { self.world.send_with_index(event, event_idx) }
    }

    /// # Panics
    ///
    /// Panics if the [`EventSet`] of this sender does not contain [`Spawn`].
    /// This may become a compile time error in the future.
    #[track_caller]
    pub fn spawn(&mut self) -> EntityId {
        let id = unsafe { self.world.queue_spawn() };
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
                EventIdx::Untargeted(i) => config.sent_untargeted_events.insert(i),
                EventIdx::Targeted(i) => config.sent_targeted_events.insert(i),
            };
        });

        Ok(state)
    }

    unsafe fn get_param<'a>(
        state: &'a mut Self::State,
        _info: &'a SystemInfo,
        _event_ptr: EventPtr<'a>,
        world: UnsafeWorldCell<'a>,
    ) -> Self::Item<'a> {
        Sender { state, world }
    }

    unsafe fn refresh_archetype(_state: &mut Self::State, _arch: &Archetype) {}

    unsafe fn remove_archetype(_state: &mut Self::State, _arch: &Archetype) {}
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
    // Either a targeted event index or an untargeted event index.
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
        #[allow(clippy::unused_unit)]
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
#[derive(Event, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct Spawn(#[event(target)] pub EntityId);

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

/// An [`Event`] sent immediately after an event is added to the world.
#[derive(Event, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct AddEvent(pub EventId);

/// An [`Event`] sent immediately before an event is removed from the world.
#[derive(Event, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct RemoveEvent(pub EventId);

#[cfg(test)]
mod tests {
    use crate::prelude::*;

    #[test]
    fn flush_entity_event() {
        let mut world = World::new();

        assert!(world.events().contains(EventId::SPAWN_QUEUED));
        assert!(world.remove_event(EventId::SPAWN_QUEUED).is_none());
    }
}
