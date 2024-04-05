//! Types for working with [`Component`]s.

use alloc::borrow::Cow;
use alloc::collections::BTreeSet;
use core::alloc::Layout;
use core::any::TypeId;
use core::ops::Index;

use ahash::RandomState;
pub use evenio_macros::Component;

use crate::archetype::{Archetype, ArchetypeIdx};
use crate::assert::UnwrapDebugChecked;
use crate::drop::DropFn;
use crate::entity::EntityLocation;
use crate::event::{Event, EventId, EventPtr};
use crate::handler::{HandlerConfig, HandlerInfo, HandlerParam, InitError};
use crate::map::{Entry, IndexSet, TypeIdMap};
use crate::prelude::World;
use crate::slot_map::{Key, SlotMap};
use crate::sparse::SparseIndex;
use crate::world::UnsafeWorldCell;

/// Contains metadata for all the components in a world.
///
/// This can be obtained in a handler by using the `&Components` handler
/// parameter.
///
/// ```
/// # use evenio::prelude::*;
/// # use evenio::component::Components;
/// #
/// # #[derive(Event)] struct E;
/// #
/// # let mut world = World::new();
/// world.add_handler(|_: Receiver<E>, components: &Components| {});
/// ```
#[derive(Debug)]
pub struct Components {
    infos: SlotMap<ComponentInfo>,
    by_type_id: TypeIdMap<ComponentId>,
}

impl Components {
    pub(crate) fn new() -> Self {
        Self {
            infos: SlotMap::new(),
            by_type_id: TypeIdMap::default(),
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

        let Some(k) = self.infos.insert_with(|k| ComponentInfo {
            name: desc.name,
            id: ComponentId(k),
            type_id: desc.type_id,
            layout: desc.layout,
            drop: desc.drop,
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
        let info = self.infos.remove(component_id.0)?;

        if let Some(type_id) = info.type_id {
            self.by_type_id.remove(&type_id);
        }

        Some(info)
    }

    /// Gets the [`ComponentInfo`] of the given component. Returns `None` if the
    /// ID is invalid.
    pub fn get(&self, id: ComponentId) -> Option<&ComponentInfo> {
        self.infos.get(id.0)
    }

    /// Gets the [`ComponentInfo`] for a component using its [`ComponentIdx`].
    /// Returns `None` if the index is invalid.
    pub fn get_by_index(&self, idx: ComponentIdx) -> Option<&ComponentInfo> {
        self.infos.get_by_index(idx.0).map(|(_, v)| v)
    }

    pub(crate) fn get_by_index_mut(&mut self, idx: ComponentIdx) -> Option<&mut ComponentInfo> {
        self.infos.get_by_index_mut(idx.0).map(|(_, v)| v)
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
        self.infos.iter().map(|(_, v)| v)
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

unsafe impl HandlerParam for &'_ Components {
    type State = ();

    type Item<'a> = &'a Components;

    fn init(_world: &mut World, _config: &mut HandlerConfig) -> Result<Self::State, InitError> {
        Ok(())
    }

    unsafe fn get<'a>(
        _state: &'a mut Self::State,
        _info: &'a HandlerInfo,
        _event_ptr: EventPtr<'a>,
        _target_location: EntityLocation,
        world: UnsafeWorldCell<'a>,
    ) -> Self::Item<'a> {
        world.components()
    }

    fn refresh_archetype(_state: &mut Self::State, _arch: &Archetype) {}

    fn remove_archetype(_state: &mut Self::State, _arch: &Archetype) {}
}

/// Metadata for a component.
#[derive(Debug)]
pub struct ComponentInfo {
    name: Cow<'static, str>,
    id: ComponentId,
    type_id: Option<TypeId>,
    layout: Layout,
    drop: DropFn,
    is_immutable: bool,
    pub(crate) insert_events: BTreeSet<EventId>,
    pub(crate) remove_events: BTreeSet<EventId>,
    /// The set of archetypes that have this component as one of its columns.
    pub(crate) member_of: IndexSet<ArchetypeIdx>,
}

impl ComponentInfo {
    /// Gets the name of the component.
    ///
    /// This name is intended for debugging purposes and should not be relied
    /// upon for correctness.
    pub fn name(&self) -> &str {
        &self.name
    }

    /// Gets the ID of the component.
    pub fn id(&self) -> ComponentId {
        self.id
    }

    /// Gets the [`TypeId`] of the component, or `None` if it was not assigned a
    /// type ID.
    pub fn type_id(&self) -> Option<TypeId> {
        self.type_id
    }

    /// Gets the [`Layout`] of the component.
    pub fn layout(&self) -> Layout {
        self.layout
    }

    /// Gets the [`DropFn`] of the component.
    pub fn drop(&self) -> DropFn {
        self.drop
    }

    /// Gets the [immutability] of the component.
    ///
    /// [immutability]: Component::IS_IMMUTABLE
    pub fn is_immutable(&self) -> bool {
        self.is_immutable
    }

    /// Gets the set of [`Insert`] events for this component.
    ///
    /// [`Insert`]: crate::event::Insert
    pub fn insert_events(&self) -> &BTreeSet<EventId> {
        &self.insert_events
    }

    /// Gets the set of [`Remove`] components for this component.
    ///
    /// [`Remove`]: crate::event::Remove
    pub fn remove_events(&self) -> &BTreeSet<EventId> {
        &self.remove_events
    }
}

/// Types which store data on [entities].
///
/// A `Component` is a piece of data which can be attached to an entity. An
/// entity can have any combination of components, but cannot have more than one
/// component of the same type.
///
/// To add a component to an entity, use the [`Insert`] event. To access
/// components from handlers, use the [`Fetcher`] handler parameter.
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

/// Data needed to create a new component.
#[derive(Clone, Debug)]
pub struct ComponentDescriptor {
    /// The name of this component.
    ///
    /// This name is intended for debugging purposes and should not be relied
    /// upon for correctness.
    pub name: Cow<'static, str>,
    /// The [`TypeId`] of this component, if any.
    pub type_id: Option<TypeId>,
    /// The [`Layout`] of the component.
    pub layout: Layout,
    /// The [`DropFn`] of the component. This is passed a pointer to the
    /// component in order to drop it.
    pub drop: DropFn,
    /// If this component is [immutable](Component::IS_IMMUTABLE).
    pub is_immutable: bool,
}

/// Lightweight identifier for a component type.
///
/// component identifiers are implemented using an [index] and a generation
/// count. The generation count ensures that IDs from removed components are
/// not reused by new components.
///
/// A component identifier is only meaningful in the [`World`] it was created
/// from. Attempting to use a component ID in a different world will have
/// unexpected results.
///
/// [index]: ComponentIdx
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default, Debug)]
pub struct ComponentId(Key);

impl ComponentId {
    /// The component ID which never identifies a live component. This is the
    /// default value for `ComponentId`.
    pub const NULL: Self = Self(Key::NULL);

    /// Creates a new component ID from an index and generation count. Returns
    /// `None` if a valid ID is not formed.
    pub const fn new(index: u32, generation: u32) -> Option<Self> {
        match Key::new(index, generation) {
            Some(k) => Some(Self(k)),
            None => None,
        }
    }

    /// Returns the index of this ID.
    pub const fn index(self) -> ComponentIdx {
        ComponentIdx(self.0.index())
    }

    /// Returns the generation count of this ID.
    pub const fn generation(self) -> u32 {
        self.0.generation().get()
    }
}

/// A [`ComponentId`] with the generation count stripped out.
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

/// An event sent immediately after a new component is added to the world.
/// Contains the ID of the added component.
#[derive(Event, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct AddComponent(pub ComponentId);

/// An event sent immediately before a component is removed from the world.
/// Contains the ID of the component to be removed.
#[derive(Event, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct RemoveComponent(pub ComponentId);

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
        let s1 = world.add_handler(|_: Receiver<E>, Single(A(a)): Single<&mut A>| {
            a.push_str("hello");
        });
        world.send(E);

        assert!(world.remove_component(c1).is_some());
        assert!(!world.handlers().contains(s1));
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
        let s2 = world.add_handler(|_: Receiver<E>, Single(B(b)): Single<&mut B>| {
            b.push("hello".into());
        });
        world.send(E);
        assert_eq!(world.get::<B>(e2), Some(&B(vec!["hello".into()])));

        assert!(world.remove_component(c2).is_some());
        assert!(!world.handlers().contains(s2));
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
