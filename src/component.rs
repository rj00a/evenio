use std::alloc::Layout;
use std::any::TypeId;
use std::mem::needs_drop;
use std::ptr::{drop_in_place, NonNull};

use evenio_macros::all_tuples;
pub use evenio_macros::Component;
use slab::Slab;

use crate::bit_set::BitSetIndex;
use crate::entity::EntityId;
use crate::event::{EventSet, Insert};
use crate::type_id_hash::TypeIdMap;

pub trait Component: Send + Sync + 'static {}

#[derive(Debug)]
pub(crate) struct Components {
    infos: Slab<ComponentInfo>,
    typeid_to_id: TypeIdMap<ComponentId>,
}

impl Components {
    pub(crate) fn new() -> Self {
        Self {
            infos: Slab::new(),
            typeid_to_id: Default::default(),
        }
    }

    #[inline]
    pub(crate) fn get(&self, id: ComponentId) -> Option<&ComponentInfo> {
        self.infos.get(id.0 as usize)
    }

    pub(crate) fn init_component<C: Component>(&mut self) -> ComponentId {
        // TODO

        self.typeid_to_id
            .get(&TypeId::of::<C>())
            .copied()
            .unwrap_or_else(|| self.add(ComponentInfo::new::<C>()))
    }

    #[track_caller]
    pub(crate) fn add(&mut self, info: ComponentInfo) -> ComponentId {
        let id = self.infos.insert(info);

        id.try_into()
            .map(ComponentId)
            .unwrap_or_else(|_| panic!("too many components added"))
    }

    pub(crate) fn remove(&mut self, id: ComponentId) -> Option<ComponentInfo> {
        self.infos.try_remove(id.0 as usize)
    }
}

#[derive(Debug)]
pub struct ComponentInfo {
    type_id: Option<TypeId>,
    layout: Layout,
    drop: Option<unsafe fn(NonNull<u8>)>,
}

impl ComponentInfo {
    pub fn new<C: Component>() -> Self {
        Self {
            type_id: Some(TypeId::of::<C>()),
            layout: Layout::new::<C>(),
            drop: needs_drop::<C>()
                .then_some(|ptr| unsafe { drop_in_place(ptr.cast::<C>().as_ptr()) }),
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
pub struct ComponentId(u32);

impl ComponentId {
    pub const NULL: Self = Self(u32::MAX);

    pub const fn to_bits(self) -> u32 {
        self.0
    }

    pub const fn from_bits(bits: u32) -> Self {
        Self(bits)
    }
}

impl Default for ComponentId {
    fn default() -> Self {
        Self::NULL
    }
}

impl BitSetIndex for ComponentId {
    fn bit_set_index(self) -> usize {
        self.0 as usize
    }

    fn from_bit_set_index(idx: usize) -> Self {
        Self(idx as u32)
    }
}

pub trait ComponentSet {
    type InsertEvents: EventSet;

    fn into_insert_events(self, entity: EntityId) -> Self::InsertEvents;
}

impl<C: Component> ComponentSet for C {
    type InsertEvents = Insert<C>;

    fn into_insert_events(self, entity: EntityId) -> Self::InsertEvents {
        Insert::new(self, entity)
    }
}

macro_rules! impl_component_set_tuple {
    ($(($C:ident, $c:ident)),*) => {
        impl<$($C: ComponentSet),*> ComponentSet for ($($C,)*) {
            type InsertEvents = ($($C::InsertEvents,)*);

            fn into_insert_events(self, _entity: EntityId) -> Self::InsertEvents {
                let ($($c,)*) = self;

                (
                    $(
                        $c.into_insert_events(_entity),
                    )*
                )
            }
        }
    }
}

all_tuples!(impl_component_set_tuple, 0, 15, C, c);
