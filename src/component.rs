use std::alloc::Layout;
use std::any::TypeId;
use std::collections::hash_map::Entry;
use std::mem::needs_drop;
use std::ptr::{drop_in_place, NonNull};

use slab::Slab;

use crate::util::TypeIdMap;

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

#[derive(Copy, Clone, Debug)]
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
            drop: needs_drop::<C>().then_some(|ptr| unsafe {
                // SAFETY: Caller guarantees pointer is valid.
                // Can't use `drop_in_place` since ptr may be unaligned.
                let _ = (ptr.as_ptr() as *mut C).read_unaligned();
            }),
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
    pub const fn to_bits(self) -> u32 {
        self.0
    }

    pub const fn from_bits(bits: u32) -> Self {
        Self(bits)
    }
}
