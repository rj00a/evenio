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
use slab::Slab;

use crate::system::{
    Access, ConflictingEventAccess, ConflictingEventType, SystemInitArgs, SystemParam,
};
use crate::util::{GetDebugChecked, TypeIdMap, UnwrapDebugChecked};
use crate::world::SystemRunArgs;

pub trait Event: Send + Sync + 'static {}

#[derive(Debug)]
pub(crate) struct Events {
    infos: Slab<EventInfo>,
    typeid_to_id: TypeIdMap<EventId>,
}

impl Events {
    pub(crate) fn new() -> Self {
        Self {
            infos: Slab::new(),
            typeid_to_id: Default::default(),
        }
    }

    #[track_caller]
    pub(crate) fn init_event<E: Event>(&mut self) -> EventId {
        match self.typeid_to_id.entry(TypeId::of::<E>()) {
            Entry::Occupied(o) => *o.get(),
            Entry::Vacant(_) => {
                let id = self.add_event(EventInfo::new::<E>());
                self.typeid_to_id.insert(TypeId::of::<E>(), id);
                id
            }
        }
    }

    #[track_caller]
    pub(crate) fn add_event(&mut self, info: EventInfo) -> EventId {
        if self.infos.len() >= EventId::NULL.0 as usize - 1 {
            panic!("too many events added")
        }

        EventId(self.infos.insert(info) as u32)
    }

    #[inline]
    pub(crate) fn event(&self, id: EventId) -> Option<&EventInfo> {
        self.infos.get(id.0 as usize)
    }
}

#[derive(Debug)]
pub struct EventInfo {
    name: Cow<'static, str>,
    type_id: Option<TypeId>,
    layout: Layout,
    drop: Option<unsafe fn(NonNull<u8>)>,
}

impl EventInfo {
    pub fn new<E: Event>() -> Self {
        Self {
            name: any::type_name::<E>().into(),
            type_id: Some(TypeId::of::<E>()),
            layout: Layout::new::<E>(),
            drop: mem::needs_drop::<E>()
                .then_some(|ptr| unsafe { ptr::drop_in_place(ptr.as_ptr()) }),
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
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct EventId(pub(crate) u32);

impl EventId {
    pub const NULL: Self = Self(u32::MAX);

    #[inline]
    pub const fn to_bits(self) -> u64 {
        self.0 as u64
    }

    #[inline]
    pub const fn from_bits(bits: u64) -> Self {
        Self(bits as u32)
    }
}

impl Default for EventId {
    fn default() -> Self {
        Self::NULL
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

    /// Push an event to the event queue.
    ///
    /// # Safety
    ///
    /// `event_id` must be correct for the given event type. Otherwise,
    /// undefined behavior may occur down the road.
    pub(crate) unsafe fn push<E: Event>(&mut self, event: E, event_id: EventId) {
        let event_ptr = NonNull::from(self.bump.alloc(event)).cast::<u8>();

        self.items.push(EventQueueItem {
            event_id,
            event_ptr,
        });
    }

    #[track_caller]
    pub(crate) unsafe fn get_unchecked(&self, idx: usize) -> EventQueueItem {
        *self.items.get_debug_checked(idx)
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

#[derive(Copy, Clone, Debug)]
pub(crate) struct EventQueueItem {
    /// Type of this event.
    pub(crate) event_id: EventId,
    /// Type-erased pointer to this event.
    pub(crate) event_ptr: NonNull<u8>,
}

impl<E: Event> SystemParam for &'_ E {
    type State = ();
    type Item<'s, 'a> = &'a E;

    fn init(args: &mut SystemInitArgs) -> Result<Self::State, Box<dyn Error>> {
        match args.event_access {
            Some(Access::Read) => {}
            None => args.event_access = Some(Access::Read),
            _ => return Err(Box::new(ConflictingEventAccess)),
        }

        let this_id = args.world.init_event::<E>();

        if args.event_id == EventId::NULL {
            args.event_id = this_id;
        } else if args.event_id != this_id {
            return Err(Box::new(ConflictingEventType));
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

    fn init(args: &mut SystemInitArgs) -> Result<Self::State, Box<dyn Error>> {
        if let Some(Access::Read | Access::ReadWrite) = args.event_access {
            return Err(Box::new(ConflictingEventAccess));
        }

        args.event_access = Some(Access::ReadWrite);

        let this_id = args.world.init_event::<E>();

        if args.event_id == EventId::NULL {
            args.event_id = this_id;
        } else if args.event_id != this_id {
            return Err(Box::new(ConflictingEventType));
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

    pub fn get(&self) -> &'a E {
        unsafe { &*self.event_ptr.unwrap_debug_checked().cast::<E>().as_ref() }
    }

    pub fn get_mut(&mut self) -> &'a mut E {
        unsafe { &mut *self.event_ptr.unwrap_debug_checked().cast::<E>().as_mut() }
    }
}

impl<E: Event> Deref for Take<'_, E> {
    type Target = E;

    fn deref(&self) -> &Self::Target {
        self.get()
    }
}

impl<E: Event> DerefMut for Take<'_, E> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.get_mut()
    }
}

impl<E> fmt::Debug for Take<'_, E>
where
    E: Event + fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("Take").field("event", &*self).finish()
    }
}

impl<E: Event> SystemParam for Take<'_, E> {
    type State = ();

    type Item<'s, 'a> = Take<'a, E>;

    fn init(args: &mut SystemInitArgs) -> Result<Self::State, Box<dyn Error>> {
        Ok(())
    }

    unsafe fn get_param<'s, 'a>(
        _state: &'s mut Self::State,
        args: SystemRunArgs<'a>,
    ) -> Option<Self::Item<'s, 'a>> {
        Some(Self::Item::new(args.event_ptr_mut()))
    }
}

pub struct Sender<E: EventSet = ()> {
    _marker: PhantomData<fn(E)>,
}

impl<E: EventSet> fmt::Debug for Sender<E> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Sender").finish()
    }
}

pub trait EventSet {
    type State;

    fn event_id_of<E: Event>(state: &Self::State) -> Option<EventId>;
}

impl<E: Event> EventSet for E {
    type State = EventId;

    #[inline]
    fn event_id_of<EE: Event>(state: &Self::State) -> Option<EventId> {
        (TypeId::of::<EE>() == TypeId::of::<E>()).then_some(*state)
    }
}

macro_rules! impl_event_set_tuple {
    ($(($E:ident, $e:ident)),*) => {
        impl<$($E: EventSet),*> EventSet for ($($E,)*) {
            type State = ($($E::State,)*);

            #[inline]
            fn event_id_of<EE: Event>(($($e,)*): &Self::State) -> Option<EventId> {
                $(
                    if let Some(id) = $E::event_id_of::<EE>($e) {
                        return Some(id);
                    }
                )*

                None
            }
        }
    };
}

all_tuples!(impl_event_set_tuple, 0, 15, E, e);
