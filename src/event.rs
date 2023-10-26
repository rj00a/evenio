use std::alloc::Layout;
use std::any::TypeId;
use std::collections::hash_map::Entry;
use std::marker::PhantomData;
use std::ops::{Deref, DerefMut};
use std::ptr::NonNull;
use std::{fmt, mem, ptr};

use evenio_macros::all_tuples;
use slab::Slab;

use crate::util::TypeIdMap;

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
    type_id: Option<TypeId>,
    layout: Layout,
    drop: Option<unsafe fn(NonNull<u8>)>,
}

impl EventInfo {
    pub fn new<E: Event>() -> Self {
        Self {
            type_id: Some(TypeId::of::<E>()),
            layout: Layout::new::<E>(),
            drop: mem::needs_drop::<E>()
                .then_some(|ptr| unsafe { ptr::drop_in_place(ptr.as_ptr()) }),
        }
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

#[derive(Copy, Clone, Debug)]
pub(crate) struct EventQueueItem {
    pub(crate) event_id: EventId,
    /// Type-erased pointer to the event in the bump allocator.
    pub(crate) event_ptr: NonNull<u8>,
}

pub struct Take<E> {
    event: NonNull<E>,
    // TODO: add lifetime, hold variable to signal taken.
}

impl<E: Event> Take<E> {
    pub(crate) unsafe fn new(event: NonNull<E>) -> Self {
        Self { event }
    }

    pub fn take(self) -> E {
        // SAFETY: Caller guarantees pointer is properly aligned and points to
        // initialized E.
        unsafe { self.event.as_ptr().read() }
    }
}

impl<E: Event> Deref for Take<E> {
    type Target = E;

    fn deref(&self) -> &Self::Target {
        unsafe { &*self.event.as_ptr() }
    }
}

impl<E: Event> DerefMut for Take<E> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { &mut *self.event.as_ptr() }
    }
}

impl<E> fmt::Debug for Take<E>
where
    E: Event + fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("Take").field("event", self.deref()).finish()
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
