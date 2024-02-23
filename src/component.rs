//! Types for working with [`Component`]s.

use alloc::borrow::Cow;
use alloc::collections::{BTreeMap, BTreeSet};
use core::alloc::Layout;
use core::any::TypeId;
use core::ops::Index;
use core::ptr::NonNull;

use ahash::RandomState;
pub use evenio_macros::Component;
use slab::Slab;

use crate::aliased_box::AliasedBox;
use crate::archetype::{Archetype, ArchetypeIdx};
use crate::assert::UnwrapDebugChecked;
use crate::drop::{drop_fn_of, DropFn};
use crate::entity::{EntityId, EntityLocation};
use crate::event::{Event, EventPtr};
use crate::map::{Entry, IndexSet, TypeIdMap};
use crate::prelude::World;
use crate::slot_map::{Key, SlotMap};
use crate::sparse::SparseIndex;
use crate::system::{Config, InitError, SystemInfo, SystemParam};
use crate::world::UnsafeWorldCell;

/// Contains metadata for all the components in a world.
///
/// This can be obtained in a system by using the `&Components` system
/// parameter.
///
/// ```
/// # use evenio::prelude::*;
/// # use evenio::component::Components;
/// #
/// # #[derive(Event)] struct E;
/// #
/// # let mut world = World::new();
/// world.add_system(|_: Receiver<E>, components: &Components| {});
/// ```
#[derive(Debug)]
pub struct Components {
    by_index: Slab<ComponentInfoPtr>,
    by_type_id: TypeIdMap<ComponentInfoPtr>,
}

impl Components {
    pub(crate) fn new() -> Self {
        Self {
            by_index: Slab::new(),
            by_type_id: TypeIdMap::default(),
        }
    }

    pub(crate) fn add(&mut self, desc: ComponentDescriptor) -> (ComponentId, bool) {
        if let Some(type_id) = desc.type_id {
            return match self.by_type_id.entry(type_id) {
                Entry::Vacant(v) => {
                    let Some(k) = self.by_index.insert_with(|k| ComponentInfo {
                        name: desc.name,
                        id: ComponentId(k),
                        type_id: desc.type_id,
                        layout: desc.layout,
                        drop: desc.drop_fn,
                        is_immutable: desc.is_immutable,
                        insert_events: BTreeSet::new(),
                        remove_events: BTreeSet::new(),
                        member_of: IndexSet::with_hasher(RandomState::new()),
                    }) else {
                        panic!("too many components")
                    };

                    (*v.insert(ComponentId(k)), true)
                }
                Entry::Occupied(o) => (*o.get(), false),
            };
        }

        let Some(k) = self.by_index.insert_with(|k| ComponentInfo {
            name: desc.name,
            id: ComponentId(k),
            type_id: desc.type_id,
            layout: desc.layout,
            drop: desc.drop_fn,
            is_immutable: desc.is_immutable,
            insert_events: BTreeSet::new(),
            remove_events: BTreeSet::new(),
            member_of: IndexSet::with_hasher(RandomState::new()),
        }) else {
            panic!("too many components")
        };

        (ComponentId(k), true)
    }

    pub(crate) fn remove(&mut self, component_id: ComponentId) -> Option<ComponentInfo> {
        let info = self.by_index.remove(component_id.0)?;

        if let Some(type_id) = info.type_id {
            self.by_type_id.remove(&type_id);
        }

        Some(info)
    }

    /// Gets the [`ComponentInfo`] of the given component. Returns `None` if the
    /// ID is invalid.
    pub fn get(&self, id: ComponentId) -> Option<&ComponentInfo> {
        self.by_index.get(id.0)
    }

    /// Gets the [`ComponentInfo`] for a component using its [`ComponentIdx`].
    /// Returns `None` if the index is invalid.
    pub fn get_by_index(&self, idx: ComponentIdx) -> Option<&ComponentInfo> {
        self.by_index.get_by_index(idx.0).map(|(_, v)| v)
    }

    pub(crate) fn get_by_index_mut(&mut self, idx: ComponentIdx) -> Option<&mut ComponentInfo> {
        self.by_index.get_by_index_mut(idx.0).map(|(_, v)| v)
    }

    /// Gets the [`ComponentInfo`] for a component using its [`TypeId`]. Returns
    /// `None` if the `TypeId` does not map to a component.
    pub fn get_by_type_id(&self, type_id: TypeId) -> Option<&ComponentInfo> {
        let id = *self.by_type_id.get(&type_id)?;
        Some(unsafe { self.get(id).unwrap_debug_checked() })
    }

    /// Does the given component exist in the world?
    pub fn contains(&self, id: ComponentId) -> bool {
        self.get(id).is_some()
    }

    /// Returns an iterator over all component infos.
    pub fn iter(&self) -> impl Iterator<Item = &ComponentInfo> {
        self.by_index.iter().map(|(_, v)| v)
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

unsafe impl SystemParam for &'_ Components {
    type State = ();

    type Item<'a> = &'a Components;

    fn init(_world: &mut World, _config: &mut Config) -> Result<Self::State, InitError> {
        Ok(())
    }

    unsafe fn get<'a>(
        _state: &'a mut Self::State,
        _info: &'a SystemInfo,
        _event_ptr: EventPtr<'a>,
        _target_location: EntityLocation,
        world: UnsafeWorldCell<'a>,
    ) -> Self::Item<'a> {
        world.components()
    }

    fn refresh_archetype(_state: &mut Self::State, _arch: &Archetype) {}

    fn remove_archetype(_state: &mut Self::State, _arch: &Archetype) {}
}

#[derive(Component, Debug)]
#[component(immutable)] // Required for soundness.
#[repr(transparent)]
pub struct ComponentInfo(AliasedBox<ComponentInfoInner>);

impl ComponentInfo {
    pub fn new<C: Component>() -> Self {
        unsafe {
            Self::from_descriptor(ComponentDescriptor {
                type_id: Some(TypeId::of::<C>()),
                layout: Layout::new::<C>(),
                drop_fn: drop_fn_of::<C>(),
                is_immutable: false,
            })
        }
    }

    pub unsafe fn from_descriptor(
        ComponentDescriptor {
            type_id,
            layout,
            drop_fn: drop,
            is_immutable,
        }: ComponentDescriptor,
    ) -> Self {
        Self(AliasedBox::new(ComponentInfoInner {
            entity_id: EntityId::NULL,
            type_id,
            layout,
            drop_fn: drop,
            is_immutable,
            insert_events: BTreeSet::new(),
            remove_events: BTreeSet::new(),
        }))
    }

    pub fn entity_id(&self) -> EntityId {
        self.0.entity_id
    }

    pub fn type_id(&self) -> Option<TypeId> {
        self.0.type_id
    }

    pub fn layout(&self) -> Layout {
        self.0.layout
    }

    pub fn drop_fn(&self) -> DropFn {
        self.0.drop_fn
    }

    pub fn is_immutable(&self) -> bool {
        self.0.is_immutable
    }
}

#[derive(Debug)]
struct ComponentInfoInner {
    entity_id: EntityId,
    type_id: Option<TypeId>,
    layout: Layout,
    drop_fn: DropFn,
    is_immutable: bool,
    insert_events: BTreeSet<EntityId>,
    remove_events: BTreeSet<EntityId>,
}

/// Data needed to create a new component.
#[derive(Clone, Debug)]
pub struct ComponentDescriptor {
    /// The [`TypeId`] of this component, if any.
    pub type_id: Option<TypeId>,
    /// The [`Layout`] of the component.
    pub layout: Layout,
    /// The [`DropFn`] of the component. This is passed a pointer to the
    /// component in order to drop it.
    pub drop_fn: DropFn,
    /// If this component is [immutable](Component::IS_IMMUTABLE).
    pub is_immutable: bool,
}

/// Pointer to component info.
#[derive(Clone, Copy, Debug)]
#[repr(transparent)]
pub(crate) struct ComponentInfoPtr(NonNull<ComponentInfoInner>);

impl ComponentInfoPtr {
    /// # Safety
    ///
    /// - Pointer must be valid.
    /// - Aliasing rules must be followed.
    pub(crate) unsafe fn as_info(&self) -> &ComponentInfo {
        // SAFETY: Both `ComponentInfo` and `ComponentInfoPtr` have non-null pointer
        // layout.
        &*(self as *const Self as *const ComponentInfo)
    }

    /// # Safety
    ///
    /// - Pointer must be valid.
    /// - Aliasing rules must be followed.
    pub(crate) unsafe fn as_info_mut(&mut self) -> &mut ComponentInfo {
        // SAFETY: Both `ComponentInfo` and `ComponentInfoPtr` have non-null pointer
        // layout.
        &mut *(self as *mut Self as *mut ComponentInfo)
    }
}

/// Types which store data on [entities].
///
/// A `Component` is a piece of data which can be attached to an entity. An
/// entity can have any combination of components, but cannot have more than one
/// component of the same type.
///
/// To add a component to an entity, use the [`Insert`] event. To access
/// components from systems, use the [`Fetcher`] system parameter.
///
/// [entities]: crate::entity
/// [`Insert`]: crate::event::Insert
/// [`Fetcher`]: crate::fetch::Fetcher
///
/// # Deriving
///
/// The `Component` trait can be implemented automatically by using the
/// associated derive macro. However, the type must still satisfy the `Send +
/// Sync + 'static` bound to do so.
///
/// ```
/// use evenio::prelude::*;
///
/// // Component with some data.
/// #[derive(Component)]
/// struct Username(String);
///
/// // Component without data, known as a "marker" or "tag" component.
/// struct Invisible;
///
/// // Derive it on structs with named fields.
/// #[derive(Component)]
/// struct Position {
///     x: f32,
///     y: f32,
///     z: f32,
/// }
///
/// // ...and on enums.
/// #[derive(Component)]
/// enum FriendStatus {
///     Friendly,
///     Neutral,
///     Unfriendly,
/// }
///
/// // Components can be immutable, which disallows mutable references
/// // to the component once it's attached to an entity.
/// #[derive(Component)]
/// #[component(immutable)] // Override the default mutability.
/// struct FooCounter(i32);
/// ```
pub trait Component: Send + Sync + 'static {
    /// Whether or not this component is immutable.
    ///
    /// Immutable components disallow mutable references, which can be used to
    /// ensure components are used in particular ways.
    const IS_IMMUTABLE: bool = false;
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

#[cfg(test)]
mod tests {
    use crate::prelude::*;

    #[derive(Event)]
    struct E;

    #[test]
    fn remove_component() {
        #[derive(Component)]
        struct A(String);

        #[derive(Component, PartialEq, Debug)]
        struct B(Vec<String>);

        let mut world = World::new();

        let c1 = world.add_component::<A>();
        let e1 = world.spawn();
        world.insert(e1, A("hello".into()));
        let s1 = world.add_system(|_: Receiver<E>, Single(A(a)): Single<&mut A>| {
            a.push_str("hello");
        });
        world.send(E);

        assert!(world.remove_component(c1).is_some());
        assert!(!world.systems().contains(s1));
        assert!(!world.entities().contains(e1));
        assert_eq!(
            world.archetypes().len(),
            1,
            "only the empty archetype should be present"
        );

        let c2 = world.add_component::<B>();
        let e2 = world.spawn();
        assert!(world.entities().contains(e2));
        world.insert(e2, B(vec![]));
        let s2 = world.add_system(|_: Receiver<E>, Single(B(b)): Single<&mut B>| {
            b.push("hello".into());
        });
        world.send(E);
        assert_eq!(world.get::<B>(e2), Some(&B(vec!["hello".into()])));

        assert!(world.remove_component(c2).is_some());
        assert!(!world.systems().contains(s2));
        assert!(!world.entities().contains(e2));
        assert_eq!(world.archetypes().len(), 1);
    }

    #[test]
    fn component_member_of() {
        let mut world = World::new();

        #[derive(Component)]
        struct A;

        #[derive(Component)]
        struct B;

        #[derive(Component)]
        struct C;

        let c1 = world.add_component::<A>();
        let c2 = world.add_component::<B>();
        let c3 = world.add_component::<C>();

        let e1 = world.spawn();
        let e2 = world.spawn();
        let e3 = world.spawn();

        world.insert(e1, A);

        world.insert(e2, A);
        world.insert(e2, B);

        world.insert(e3, A);
        world.insert(e3, B);
        world.insert(e3, C);

        assert_eq!(world.components()[c1].member_of.len(), 3);
        assert_eq!(world.components()[c2].member_of.len(), 2);
        assert_eq!(world.components()[c3].member_of.len(), 1);

        world.remove_component(c3);

        assert_eq!(world.components()[c1].member_of.len(), 2);
        assert_eq!(world.components()[c2].member_of.len(), 1);

        world.remove_component(c2);

        assert_eq!(world.components()[c1].member_of.len(), 1);
    }
}
