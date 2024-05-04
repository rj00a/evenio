use alloc::borrow::Cow;
use core::alloc::Layout;
use core::any::TypeId;
use core::ops::Index;

pub use evenio_macros::TargetedEvent;

use super::global::GlobalEvent;
use super::{Event, EventDescriptor, EventKind, EventPtr};
use crate::archetype::Archetype;
use crate::drop::DropFn;
use crate::entity::EntityLocation;
use crate::handler::{HandlerConfig, HandlerInfo, HandlerParam, InitError};
use crate::map::{Entry, TypeIdMap};
use crate::slot_map::{Key, SlotMap};
use crate::sparse::SparseIndex;
use crate::world::{UnsafeWorldCell, World};

pub trait TargetedEvent: Event<EventIdx = TargetedEventIdx> {}
impl<E: Event<EventIdx = TargetedEventIdx>> TargetedEvent for E {}

/// Stores metadata for all [`Event`]s in the world.
///
/// This can be obtained in a handler by using the `&Events` handler
/// parameter.
///
/// ```
/// # use evenio::prelude::*;
/// # use evenio::event::Events;
/// #
/// # #[derive(Event)] struct E;
/// #
/// # let mut world = World::new();
/// world.add_handler(|_: Receiver<E>, events: &Events| {});
#[derive(Debug)]
pub struct TargetedEvents {
    infos: SlotMap<TargetedEventInfo>,
    by_type_id: TypeIdMap<TargetedEventId>,
}

impl TargetedEvents {
    pub(crate) fn new() -> Self {
        Self {
            infos: SlotMap::new(),
            by_type_id: TypeIdMap::default(),
        }
    }

    pub(crate) fn add(&mut self, desc: EventDescriptor) -> (TargetedEventId, bool) {
        let info = TargetedEventInfo {
            id: TargetedEventId::NULL,
            name: desc.name,
            kind: desc.kind,
            type_id: desc.type_id,
            layout: desc.layout,
            drop: desc.drop,
            is_immutable: desc.is_immutable,
        };

        let insert = || {
            let Some(k) = self.infos.insert(info) else {
                panic!("too many targeted events")
            };
            let id = TargetedEventId(k);
            self.infos[k].id = id;
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

    /// Gets the [`EventInfo`] of the given event. Returns `None` if the ID is
    /// invalid.
    pub fn get(&self, id: TargetedEventId) -> Option<&TargetedEventInfo> {
        self.infos.get(id.0)
    }

    /// Gets the [`EventInfo`] for an event using its [`EventIdx`]. Returns
    /// `None` if the index is invalid.
    #[inline]
    pub fn get_by_index(&self, idx: TargetedEventIdx) -> Option<&TargetedEventInfo> {
        Some(self.infos.get_by_index(idx.0)?.1)
    }

    /// Gets the [`EventInfo`] for an event using its [`TypeId`]. Returns `None`
    /// if the `TypeId` does not map to an event.
    pub fn get_by_type_id(&self, type_id: TypeId) -> Option<&TargetedEventInfo> {
        let idx = *self.by_type_id.get(&type_id)?;
        Some(unsafe { self.get(idx).unwrap_unchecked() })
    }

    /// Does the given event exist in the world?
    pub fn contains(&self, id: TargetedEventId) -> bool {
        self.get(id).is_some()
    }

    pub(crate) fn remove(&mut self, id: TargetedEventId) -> Option<TargetedEventInfo> {
        let info = self.infos.remove(id.0)?;

        if let Some(type_id) = info.type_id {
            self.by_type_id.remove(&type_id);
        }

        Some(info)
    }

    /// Returns an iterator over all event infos.
    pub fn iter(&self) -> impl Iterator<Item = &TargetedEventInfo> {
        self.infos.iter().map(|(_, v)| v)
    }
}

impl Index<TargetedEventId> for TargetedEvents {
    type Output = TargetedEventInfo;

    fn index(&self, index: TargetedEventId) -> &Self::Output {
        if let Some(info) = self.get(index) {
            info
        } else {
            panic!("no such targeted event with ID of {index:?} exists")
        }
    }
}

impl Index<TargetedEventIdx> for TargetedEvents {
    type Output = TargetedEventInfo;

    fn index(&self, index: TargetedEventIdx) -> &Self::Output {
        if let Some(info) = self.get_by_index(index) {
            info
        } else {
            panic!("no such targeted event with index of {index:?} exists")
        }
    }
}

impl Index<TypeId> for TargetedEvents {
    type Output = TargetedEventInfo;

    fn index(&self, index: TypeId) -> &Self::Output {
        if let Some(info) = self.get_by_type_id(index) {
            info
        } else {
            panic!("no such targeted event with type ID of {index:?} exists")
        }
    }
}

unsafe impl HandlerParam for &'_ TargetedEvents {
    type State = ();

    type This<'a> = &'a TargetedEvents;

    fn init(_world: &mut World, _config: &mut HandlerConfig) -> Result<Self::State, InitError> {
        Ok(())
    }

    unsafe fn get<'a>(
        _state: &'a mut Self::State,
        _info: &'a HandlerInfo,
        _event_ptr: EventPtr<'a>,
        _target_location: EntityLocation,
        world: UnsafeWorldCell<'a>,
    ) -> Self::This<'a> {
        world.targeted_events()
    }

    fn refresh_archetype(_state: &mut Self::State, _arch: &Archetype) {}

    fn remove_archetype(_state: &mut Self::State, _arch: &Archetype) {}
}

#[derive(Debug)]
pub struct TargetedEventInfo {
    name: Cow<'static, str>,
    id: TargetedEventId,
    kind: EventKind,
    type_id: Option<TypeId>,
    layout: Layout,
    drop: DropFn,
    is_immutable: bool,
}

impl TargetedEventInfo {
    /// Gets the name of the event.
    ///
    /// This name is intended for debugging purposes and should not be relied
    /// upon for correctness.
    pub fn name(&self) -> &str {
        &self.name
    }

    /// Gets the ID of the event.
    pub fn id(&self) -> TargetedEventId {
        self.id
    }

    /// Gets the [`EventKind`] of the event.
    pub fn kind(&self) -> EventKind {
        self.kind
    }

    /// Gets the [`TypeId`] of the event, if any.
    pub fn type_id(&self) -> Option<TypeId> {
        self.type_id
    }

    /// Gets the [`Layout`] of the event.
    pub fn layout(&self) -> Layout {
        self.layout
    }

    /// Gets the [`DropFn`] of the event.
    pub fn drop(&self) -> DropFn {
        self.drop
    }

    /// Gets the [immutability] of the event.
    ///
    /// [immutability]: Event::IS_IMMUTABLE
    pub fn is_immutable(&self) -> bool {
        self.is_immutable
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Default, Hash, Debug)]
pub struct TargetedEventId(Key);

impl TargetedEventId {
    /// The global event ID which never identifies a live entity. This is the
    /// default value for `EntityId`.
    pub const NULL: Self = Self(Key::NULL);

    /// Creates a new entity ID from an index and generation count. Returns
    /// `None` if a valid ID is not formed.
    pub const fn new(index: u32, generation: u32) -> Option<Self> {
        match Key::new(index, generation) {
            Some(k) => Some(Self(k)),
            None => None,
        }
    }

    /// Returns the index of this ID.
    pub const fn index(self) -> TargetedEventIdx {
        TargetedEventIdx(self.0.index())
    }

    /// Returns the generation count of this ID.
    pub const fn generation(self) -> u32 {
        self.0.generation().get()
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

/// An [`Event`] sent immediately after a new targeted event is added to the
/// world.
///
/// Contains the [`TargetedEventId`] of the added event.
#[derive(GlobalEvent, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct AddTargetedEvent(pub TargetedEventId);

/// An [`Event`] sent immediately before a targeted event is removed from the
/// world.
///
/// Contains the [`TargetedEventId`] of the event to be removed.
#[derive(GlobalEvent, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct RemoveTargetedEvent(pub TargetedEventId);
