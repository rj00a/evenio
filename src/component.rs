use alloc::collections::BTreeMap;
use core::alloc::Layout;
use core::any::TypeId;
use core::marker::PhantomData;
use std::borrow::Cow;
use std::collections::btree_map::Entry;
use std::ops::Index;

pub use evenio_macros::Component;

use crate::debug_checked::UnwrapDebugChecked;
use crate::event::{Event, EventPtr};
use crate::prelude::World;
use crate::slot_map::{Key, SlotMap};
use crate::sparse::SparseIndex;
use crate::system::{Config, InitError, SystemInfo, SystemParam};
use crate::world::UnsafeWorldCell;
use crate::DropFn;

#[derive(Debug)]
pub struct Components {
    infos: SlotMap<ComponentInfo>,
    by_type_id: BTreeMap<TypeId, ComponentId>,
}

impl Components {
    pub(crate) fn new() -> Self {
        Self {
            infos: SlotMap::new(),
            by_type_id: BTreeMap::new(),
        }
    }

    pub(crate) fn add(&mut self, desc: ComponentDescriptor) -> (ComponentId, bool) {
        if let Some(type_id) = desc.type_id {
            return match self.by_type_id.entry(type_id) {
                Entry::Vacant(v) => {
                    let Some(k) = self.infos.insert_with(|k| ComponentInfo {
                        name: desc.name,
                        id: ComponentId(k),
                        type_id: desc.type_id,
                        layout: desc.layout,
                        drop: desc.drop,
                    }) else {
                        panic!("too many components")
                    };

                    (*v.insert(ComponentId(k)), true)
                }
                Entry::Occupied(o) => (*o.get(), false),
            };
        }

        let Some(k) = self.infos.insert_with(|k| ComponentInfo {
            name: desc.name,
            id: ComponentId(k),
            type_id: desc.type_id,
            layout: desc.layout,
            drop: desc.drop,
        }) else {
            panic!("too many components")
        };

        (ComponentId(k), true)
    }

    pub fn get(&self, id: ComponentId) -> Option<&ComponentInfo> {
        self.infos.get(id.0)
    }

    pub fn get_by_index(&self, idx: ComponentIdx) -> Option<&ComponentInfo> {
        self.infos.get_by_index(idx.0).map(|(_, v)| v)
    }

    pub fn get_by_type_id(&self, type_id: TypeId) -> Option<&ComponentInfo> {
        let id = *self.by_type_id.get(&type_id)?;
        Some(unsafe { self.get(id).unwrap_debug_checked() })
    }

    pub fn contains(&self, id: ComponentId) -> bool {
        self.get(id).is_some()
    }
}

impl Index<ComponentId> for Components {
    type Output = ComponentInfo;

    fn index(&self, index: ComponentId) -> &Self::Output {
        if let Some(info) = self.get(index) {
            info
        } else {
            panic!("no such component with ID of {index:?} exists")
        }
    }
}

impl Index<ComponentIdx> for Components {
    type Output = ComponentInfo;

    fn index(&self, index: ComponentIdx) -> &Self::Output {
        if let Some(info) = self.get_by_index(index) {
            info
        } else {
            panic!("no such component with index of {index:?} exists")
        }
    }
}

impl Index<TypeId> for Components {
    type Output = ComponentInfo;

    fn index(&self, index: TypeId) -> &Self::Output {
        if let Some(info) = self.get_by_type_id(index) {
            info
        } else {
            panic!("no such component with type ID of {index:?} exists")
        }
    }
}

impl SystemParam for &'_ Components {
    type State = ();

    type Item<'a> = &'a Components;

    fn init(_world: &mut World, _config: &mut Config) -> Result<Self::State, InitError> {
        Ok(())
    }

    unsafe fn get_param<'a>(
        _state: &'a mut Self::State,
        _info: &'a SystemInfo,
        _event_ptr: EventPtr<'a>,
        world: UnsafeWorldCell<'a>,
    ) -> Self::Item<'a> {
        world.components()
    }

    unsafe fn refresh_archetype(_state: &mut Self::State, _arch: &crate::archetype::Archetype) {}

    unsafe fn remove_archetype(_state: &mut Self::State, _arch: &crate::archetype::Archetype) {}
}

#[derive(Debug)]
pub struct ComponentInfo {
    name: Cow<'static, str>,
    id: ComponentId,
    type_id: Option<TypeId>,
    layout: Layout,
    drop: DropFn,
}

impl ComponentInfo {
    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn id(&self) -> ComponentId {
        self.id
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

/// # Deriving
///
/// ```
/// use evenio::prelude::*;
///
/// #[derive(Component)]
/// #[component(is_mutable = false)] // Override the default mutability.
/// struct MyComponent(i32);
/// ```
pub trait Component: Send + Sync + 'static {
    const IS_MUTABLE: bool = true;
}

#[derive(Clone, Debug)]
pub struct ComponentDescriptor {
    pub name: Cow<'static, str>,
    pub type_id: Option<TypeId>,
    pub layout: Layout,
    pub drop: DropFn,
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

#[derive(Event, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct AddComponent(pub ComponentId);

#[derive(Event, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct RemoveComponent(pub ComponentId);

pub(crate) struct AssertMutable<C>(PhantomData<C>);

impl<C: Component> AssertMutable<C> {
    pub(crate) const ASSERTION: () = assert!(
        C::IS_MUTABLE,
        "component does not permit mutation through mutable references (see \
         `Component::IS_MUTABLE`)."
    );
}
