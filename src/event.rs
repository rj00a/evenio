use std::alloc::Layout;
use std::any::{self, TypeId};
use std::borrow::Cow;
use std::collections::hash_map::Entry;
use std::error::Error;
use std::marker::PhantomData;
use std::ops::{Deref, DerefMut};
use std::ptr::NonNull;
use std::{fmt, mem, ptr};

use bumpalo::Bump;
use evenio_macros::all_tuples;
pub use evenio_macros::Event;

use crate::component::ComponentId;
use crate::entity::EntityId;
use crate::id::{Ident, Storage};
use crate::prelude::{Component, World};
use crate::system::{Access, SystemInitArgs, SystemInitError, SystemListEntry, SystemParam};
use crate::util::{GetDebugChecked, TypeIdMap, UnwrapDebugChecked};
use crate::world::SystemRunArgs;

pub unsafe trait Event: Send + Sync + 'static {
    fn init(_world: &mut World) -> EventKind {
        EventKind::Other
    }
}

#[derive(Debug)]
pub(crate) struct Events {
    infos: Storage<EventInfo>,
    typeid_to_id: TypeIdMap<EventId>,
}

impl Events {
    pub(crate) fn new() -> Self {
        Self {
            infos: Storage::new(),
            typeid_to_id: Default::default(),
        }
    }

    #[track_caller]
    pub(crate) unsafe fn init<E: Event>(&mut self, kind: EventKind) -> EventId {
        match self.typeid_to_id.entry(TypeId::of::<E>()) {
            Entry::Occupied(o) => *o.get(),
            Entry::Vacant(_) => {
                let mut info = EventInfo::new::<E>();
                info.kind = kind;

                let id = self.add(info);
                self.typeid_to_id.insert(TypeId::of::<E>(), id);
                id
            }
        }
    }

    #[track_caller]
    pub(crate) unsafe fn add(&mut self, info: EventInfo) -> EventId {
        EventId(self.infos.add(info))
    }

    #[inline]
    pub(crate) fn get(&self, id: EventId) -> Option<&EventInfo> {
        self.infos.get(id.0)
    }

    #[inline]
    pub(crate) unsafe fn get_unchecked(&self, id: EventId) -> &EventInfo {
        self.infos.get_unchecked(id.0)
    }
}

#[derive(Debug)]
pub struct EventInfo {
    name: Cow<'static, str>,
    type_id: Option<TypeId>,
    layout: Layout,
    drop: Option<unsafe fn(NonNull<u8>)>,
    kind: EventKind,
}

impl EventInfo {
    pub fn new<E: Event>() -> Self {
        Self {
            name: any::type_name::<E>().into(),
            type_id: Some(TypeId::of::<E>()),
            layout: Layout::new::<E>(),
            drop: mem::needs_drop::<E>()
                .then_some(|ptr| unsafe { ptr::drop_in_place(ptr.as_ptr().cast::<E>()) }),
            kind: EventKind::Other,
        }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn type_id(&self) -> Option<TypeId> {
        self.type_id
    }

    pub fn layout(&self) -> Layout {
        self.layout
    }

    pub fn drop(&self) -> Option<unsafe fn(NonNull<u8>)> {
        self.drop
    }

    pub fn kind(&self) -> EventKind {
        self.kind
    }
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Default, Debug)]
pub struct EventId(Ident);

impl EventId {
    pub const NULL: Self = Self(Ident::NULL);

    pub(crate) fn index(&self) -> u32 {
        self.0.index
    }

    #[inline]
    pub const fn to_bits(self) -> u64 {
        self.0.to_bits()
    }

    #[inline]
    pub const fn from_bits(bits: u64) -> Option<Self> {
        match Ident::from_bits(bits) {
            Some(id) => Some(Self(id)),
            None => None,
        }
    }
}

#[derive(Debug)]
pub struct EventQueue {
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

    /// Push an event to the event queue.
    ///
    /// # Safety
    ///
    /// `event_id` must be correct for the given event type. Otherwise,
    /// undefined behavior may occur down the road.
    pub unsafe fn push<E: Event>(&mut self, event: E, event_id: EventId) {
        let event_ptr = NonNull::from(self.bump.alloc(event)).cast::<u8>();

        self.items.push(EventQueueItem {
            event_id,
            event_ptr: Some(event_ptr),
            system_slice: &mut [],
            system_idx: 0,
        });
    }

    pub(crate) unsafe fn get_unchecked_mut(&mut self, idx: usize) -> &mut EventQueueItem {
        self.items.get_debug_checked_mut(idx)
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

    pub(crate) fn iter(&self) -> impl Iterator<Item = &EventQueueItem> + '_ {
        self.items.iter()
    }
}

// SAFETY: The bump allocator is only accessed behind an exclusive reference to
// the event queue.
unsafe impl Sync for EventQueue {}

#[derive(Copy, Clone, Debug)]
pub(crate) struct EventQueueItem {
    /// Type of this event.
    pub(crate) event_id: EventId,
    /// Type-erased pointer to this event. When `None`, ownership of the event
    /// has been transferred and no destructor needs to run.
    pub(crate) event_ptr: Option<NonNull<u8>>,
    /// Slice of systems that handle this event.
    pub(crate) system_slice: *mut [SystemListEntry],
    /// Current index into the system slice.
    pub(crate) system_idx: u32,
}

impl<E: Event> SystemParam for &'_ E {
    type State = ();
    type Item<'s, 'a> = &'a E;

    fn init(args: &mut SystemInitArgs<'_>) -> Result<Self::State, Box<dyn Error>> {
        match args.event_access {
            Some(Access::Read) => {}
            None => args.event_access = Some(Access::Read),
            _ => return Err(Box::new(SystemInitError::ConflictingEventAccess)),
        }

        let this_id = args.world.init_event::<E>();

        if args.event_id == EventId::NULL {
            args.event_id = this_id;
        } else if args.event_id != this_id {
            return Err(Box::new(SystemInitError::ConflictingEventType));
        }

        Ok(())
    }

    #[inline]
    unsafe fn get_param<'s, 'a>(
        _state: &'s mut Self::State,
        args: SystemRunArgs<'a>,
    ) -> Option<Self::Item<'s, 'a>> {
        // SAFETY:
        // - Access to event pointer is shared.
        // - Event pointer is initialized as Some, so unwrap will not fail.
        let ptr = args.event_ptr().unwrap_debug_checked().cast::<E>();

        // SAFETY: Caller guarantees pointer is valid and properly aligned.
        Some(&*ptr.as_ref())
    }
}

impl<E: Event> SystemParam for &'_ mut E {
    type State = ();
    type Item<'s, 'a> = &'a mut E;

    fn init(args: &mut SystemInitArgs<'_>) -> Result<Self::State, Box<dyn Error>> {
        if let Some(Access::Read | Access::ReadWrite) = args.event_access {
            return Err(Box::new(SystemInitError::ConflictingEventAccess));
        }

        args.event_access = Some(Access::ReadWrite);

        let this_id = args.world.init_event::<E>();

        if args.event_id == EventId::NULL {
            args.event_id = this_id;
        } else if args.event_id != this_id {
            return Err(Box::new(SystemInitError::ConflictingEventType));
        }

        Ok(())
    }

    unsafe fn get_param<'s, 'a>(
        _state: &'s mut Self::State,
        args: SystemRunArgs<'a>,
    ) -> Option<Self::Item<'s, 'a>> {
        let mut ptr = args.event_ptr_mut().unwrap_debug_checked().cast::<E>();

        Some(&mut *ptr.as_mut())
    }
}

pub struct Take<'a, E> {
    event_ptr: &'a mut Option<NonNull<u8>>,
    _marker: PhantomData<&'a mut Option<E>>,
}

impl<'a, E: Event> Take<'a, E> {
    pub(crate) unsafe fn new(event_ptr: &'a mut Option<NonNull<u8>>) -> Self {
        Self {
            event_ptr,
            _marker: PhantomData,
        }
    }

    pub fn take(self) -> E {
        let ptr = unsafe { mem::take(self.event_ptr).unwrap_debug_checked() }.cast::<E>();

        unsafe { ptr::read(ptr.as_ptr()) }
    }
}

impl<E: Event> Deref for Take<'_, E> {
    type Target = E;

    fn deref(&self) -> &Self::Target {
        unsafe { &*self.event_ptr.unwrap_debug_checked().cast::<E>().as_ref() }
    }
}

impl<E: Event> DerefMut for Take<'_, E> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { &mut *self.event_ptr.unwrap_debug_checked().cast::<E>().as_mut() }
    }
}

impl<E> fmt::Debug for Take<'_, E>
where
    E: Event + fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Take").field("event", &*self).finish()
    }
}

impl<E: Event> SystemParam for Take<'_, E> {
    type State = ();

    type Item<'s, 'a> = Take<'a, E>;

    fn init(args: &mut SystemInitArgs<'_>) -> Result<Self::State, Box<dyn Error>> {
        // TODO: register accessed things.
        Ok(())
    }

    unsafe fn get_param<'s, 'a>(
        _state: &'s mut Self::State,
        args: SystemRunArgs<'a>,
    ) -> Option<Self::Item<'s, 'a>> {
        Some(Self::Item::new(args.event_ptr_mut()))
    }
}

pub struct Sender<'s, 'a, Es: EventSet> {
    state: &'s Es::State,
    event_queue: &'a mut EventQueue,
}

impl<Es: EventSet> Sender<'_, '_, Es> {
    /// # Panics
    ///
    /// Panics if the given event type `E` is not in the `EventSet` of this
    /// sender. This may become a compile time error in the future.
    pub fn send<E: Event>(&mut self, event: E) {
        // The event type and event set are all compile time known, so the compiler
        // should be able to optimize this away.
        let event_id = Es::event_id_of::<E>(self.state).unwrap_or_else(|| {
            panic!(
                "event {} is not in the EventSet of this Sender",
                any::type_name::<E>()
            )
        });

        unsafe {
            self.event_queue.push(event, event_id);
        }
    }

    // TODO: unsafe fn send_raw or similar.
}

impl<Es: EventSet> fmt::Debug for Sender<'_, '_, Es> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Sender").finish()
    }
}

impl<Es: EventSet> SystemParam for Sender<'_, '_, Es> {
    type State = Es::State;

    type Item<'s, 'a> = Sender<'s, 'a, Es>;

    fn init(args: &mut SystemInitArgs<'_>) -> Result<Self::State, Box<dyn Error>> {
        if let Some(Access::Read | Access::ReadWrite) = args.event_queue_access {
            return Err(Box::new(SystemInitError::ConflictingEventQueueAccess));
        }

        args.event_queue_access = Some(Access::ReadWrite);

        Ok(Es::new_state(args.world))
    }

    unsafe fn get_param<'s, 'a>(
        state: &'s mut Self::State,
        args: SystemRunArgs<'a>,
    ) -> Option<Self::Item<'s, 'a>> {
        Some(Sender {
            state,
            event_queue: unsafe { args.event_queue_mut() },
        })
    }
}

pub trait EventSet {
    type State: Send + Sync + 'static;

    fn new_state(world: &mut World) -> Self::State;

    fn event_id_of<E: Event>(state: &Self::State) -> Option<EventId>;
}

impl<E: Event> EventSet for E {
    type State = EventId;

    fn new_state(world: &mut World) -> Self::State {
        world.init_event::<E>()
    }

    #[inline]
    fn event_id_of<EE: Event>(state: &Self::State) -> Option<EventId> {
        (TypeId::of::<EE>() == TypeId::of::<E>()).then_some(*state)
    }
}

macro_rules! impl_event_set_tuple {
    ($(($E:ident, $e:ident)),*) => {
        impl<$($E: EventSet),*> EventSet for ($($E,)*) {
            type State = ($($E::State,)*);

            fn new_state(_world: &mut World) -> Self::State {
                (
                    $(
                        $E::new_state(_world),
                    )*
                )
            }

            #[inline]
            fn event_id_of<E: Event>(($($e,)*): &Self::State) -> Option<EventId> {
                $(
                    if let Some(id) = $E::event_id_of::<E>($e) {
                        return Some(id);
                    }
                )*

                None
            }
        }
    };
}

all_tuples!(impl_event_set_tuple, 0, 15, E, e);

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default, Debug)]
pub struct Spawn(pub EntityId);

unsafe impl Event for Spawn {
    fn init(_world: &mut World) -> EventKind {
        EventKind::Spawn
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default, Debug)]
pub struct Insert<C> {
    pub component: C,
    pub entity: EntityId,
}

impl<C> Insert<C> {
    pub const fn new(component: C, entity: EntityId) -> Self {
        Self { component, entity }
    }
}

unsafe impl<C: Component> Event for Insert<C> {
    fn init(world: &mut World) -> EventKind {
        let id = world.init_component::<C>();
        EventKind::InsertComponent(id)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default, Debug)]
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
    fn init(world: &mut World) -> EventKind {
        let id = world.init_component::<C>();
        EventKind::RemoveComponent(id)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default, Debug)]
pub struct Despawn(pub EntityId);

unsafe impl Event for Despawn {
    fn init(_world: &mut World) -> EventKind {
        EventKind::Despawn
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default, Debug)]
pub struct Flush;

unsafe impl Event for Flush {
    fn init(_world: &mut World) -> EventKind {
        EventKind::FlushCommands
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default, Debug)]
pub enum EventKind {
    #[default]
    Other,
    Spawn,
    InsertComponent(ComponentId),
    RemoveComponent(ComponentId),
    Despawn,
    FlushCommands,
}
