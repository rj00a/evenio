use alloc::collections::BTreeMap;
use core::alloc::Layout;
use core::any::TypeId;
use core::marker::PhantomData;
use std::borrow::Cow;
use std::ptr::NonNull;

pub use evenio_macros::Component;

use crate::slot_map::{Key, SlotMap};
use crate::sparse::SparseIndex;

#[derive(Debug)]
pub struct Components {
    sm: SlotMap<ComponentInfo>,
    by_type_id: BTreeMap<TypeId, ComponentIdx>,
}

impl Components {
    pub(crate) fn new() -> Self {
        Self {
            sm: SlotMap::new(),
            by_type_id: BTreeMap::new(),
        }
    }

    pub fn get(&self, id: ComponentId) -> Option<&ComponentInfo> {
        self.sm.get(id.0)
    }
}

#[derive(Debug)]
pub struct ComponentInfo {
    name: Cow<'static, str>,
    id: ComponentId,
    type_id: Option<TypeId>,
    layout: Layout,
    drop: Option<unsafe fn(NonNull<u8>)>,
}

pub trait Component: Send + Sync + 'static {
    /// If this component permits mutation through mutable references (`&mut
    /// T`).
    ///
    /// ```compile_fail
    /// use evenio::prelude::*;
    ///
    /// struct MyComponent;
    ///
    /// impl Component for MyComponent {
    ///     const MUTABLE: bool = false;
    /// }
    ///
    /// fn my_system(_: Fetcher<&mut MyComponent>) {}
    ///
    /// let mut world = World::new();
    ///
    /// // Fails to compile because `&mut MyComponent` is not allowed.
    /// world.add_system(my_system);
    /// ```
    const MUTABLE: bool = true;
}

#[derive(Clone, Debug)]
pub struct ComponentDescriptor {
    pub name: Cow<'static, str>,
    pub type_id: Option<TypeId>,
    pub layout: Layout,
    pub drop: Option<unsafe fn(NonNull<u8>)>,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default, Debug)]
pub struct ComponentId(Key);

impl ComponentId {
    pub const NULL: Self = Self(Key::NULL);

    pub const fn index(self) -> ComponentIdx {
        ComponentIdx(self.0.index())
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default, Debug)]
pub struct ComponentIdx(pub u32);

unsafe impl SparseIndex for ComponentIdx {
    const MAX: Self = Self(u32::MAX);

    fn index(self) -> usize {
        self.0.index()
    }

    fn from_index(idx: usize) -> Self {
        Self(u32::from_index(idx))
    }
}
