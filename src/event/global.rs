use alloc::borrow::Cow;
use core::alloc::Layout;
use core::any::TypeId;
use core::ops::Index;

pub use evenio_macros::GlobalEvent;

use super::{Event, EventDescriptor, EventKind, EventPtr, Mutability};
use crate::archetype::Archetype;
use crate::drop::DropFn;
use crate::entity::EntityLocation;
use crate::handler::{HandlerConfig, HandlerInfo, HandlerParam, InitError};
use crate::map::{Entry, TypeIdMap};
use crate::slot_map::{Key, SlotMap};
use crate::sparse::SparseIndex;
use crate::world::{UnsafeWorldCell, World};

/// An event which has no target entity.
///
/// This trait is automatically implemented for all types which implement
/// `Event<EventIdx = GlobalEventIdx>`. Use the derive macro to create the
/// appropriate implementation of [`Event`].
///
/// Note that this trait is intended to be mutually exclusive with
/// [`TargetedEvent`](super::TargetedEvent).
///
/// # Deriving
///
/// ```
/// use evenio::prelude::*;
///
/// #[derive(GlobalEvent)]
/// struct MyEvent {
///     example_data: i32,
/// }
/// ```
pub trait GlobalEvent: Event<EventIdx = GlobalEventIdx> {}
impl<E: Event<EventIdx = GlobalEventIdx>> GlobalEvent for E {}

/// Stores metadata for all [`GlobalEvent`]s in the world.
///
/// This can be obtained in a handler by using the `&GlobalEvents` handler
/// parameter.
///
/// ```
/// # use evenio::prelude::*;
/// # use evenio::event::GlobalEvents;
/// #
/// # #[derive(GlobalEvent)] struct E;
/// #
/// # let mut world = World::new();
/// world.add_handler(|_: Receiver<E>, events: &GlobalEvents| {});
#[derive(Debug)]
pub struct GlobalEvents {
    infos: SlotMap<GlobalEventInfo>,
    by_type_id: TypeIdMap<GlobalEventId>,
}

impl GlobalEvents {
    pub(crate) fn new() -> Self {
        Self {
            infos: SlotMap::new(),
            by_type_id: TypeIdMap::default(),
        }
    }

    pub(crate) fn add(&mut self, desc: EventDescriptor) -> (GlobalEventId, bool) {
        let mut info = GlobalEventInfo {
            id: GlobalEventId::NULL,
            name: desc.name,
            kind: desc.kind,
            type_id: desc.type_id,
            layout: desc.layout,
            drop: desc.drop,
            mutability: desc.mutability,
        };

        let insert = || {
            GlobalEventId(
                self.infos
                    .insert_with(|id| {
                        info.id = GlobalEventId(id);
                        info
                    })
                    .expect("too many global events"),
            )
        };

        if let Some(type_id) = desc.type_id {
            match self.by_type_id.entry(type_id) {
                Entry::Vacant(v) => (*v.insert(insert()), true),
                Entry::Occupied(o) => (*o.get(), false),
            }
        } else {
            (insert(), true)
        }
    }

    /// Gets the [`GlobalEventInfo`] of the given event. Returns `None` if the
    /// ID is invalid.
    pub fn get(&self, id: GlobalEventId) -> Option<&GlobalEventInfo> {
        self.infos.get(id.0)
    }

    /// Gets the [`GlobalEventInfo`] for an event using its [`GlobalEventIdx`].
    /// Returns `None` if the index is invalid.
    #[inline]
    pub fn get_by_index(&self, idx: GlobalEventIdx) -> Option<&GlobalEventInfo> {
        Some(self.infos.get_by_index(idx.0)?.1)
    }

    /// Gets the [`GlobalEventInfo`] for an event using its [`TypeId`]. Returns
    /// `None` if the `TypeId` does not map to an event.
    pub fn get_by_type_id(&self, type_id: TypeId) -> Option<&GlobalEventInfo> {
        let idx = *self.by_type_id.get(&type_id)?;
        Some(unsafe { self.get(idx).unwrap_unchecked() })
    }

    /// Does the given event exist in the world?
    pub fn contains(&self, id: GlobalEventId) -> bool {
        self.get(id).is_some()
    }

    pub(crate) fn remove(&mut self, id: GlobalEventId) -> Option<GlobalEventInfo> {
        let info = self.infos.remove(id.0)?;

        if let Some(type_id) = info.type_id {
            self.by_type_id.remove(&type_id);
        }

        Some(info)
    }

    /// Returns an iterator over all event infos.
    pub fn iter(&self) -> impl Iterator<Item = &GlobalEventInfo> {
        self.infos.iter().map(|(_, v)| v)
    }
}

impl Index<GlobalEventId> for GlobalEvents {
    type Output = GlobalEventInfo;

    fn index(&self, index: GlobalEventId) -> &Self::Output {
        if let Some(info) = self.get(index) {
            info
        } else {
            panic!("no such global event with ID of {index:?} exists")
        }
    }
}

impl Index<GlobalEventIdx> for GlobalEvents {
    type Output = GlobalEventInfo;

    fn index(&self, index: GlobalEventIdx) -> &Self::Output {
        if let Some(info) = self.get_by_index(index) {
            info
        } else {
            panic!("no such global event with index of {index:?} exists")
        }
    }
}

impl Index<TypeId> for GlobalEvents {
    type Output = GlobalEventInfo;

    fn index(&self, index: TypeId) -> &Self::Output {
        if let Some(info) = self.get_by_type_id(index) {
            info
        } else {
            panic!("no such global event with type ID of {index:?} exists")
        }
    }
}

unsafe impl HandlerParam for &'_ GlobalEvents {
    type State = ();

    type This<'a> = &'a GlobalEvents;

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
        world.global_events()
    }

    fn refresh_archetype(_state: &mut Self::State, _arch: &Archetype) {}

    fn remove_archetype(_state: &mut Self::State, _arch: &Archetype) {}
}

/// Contains all the metadata for an added [`GlobalEvent`].
#[derive(Debug)]
pub struct GlobalEventInfo {
    name: Cow<'static, str>,
    id: GlobalEventId,
    kind: EventKind,
    type_id: Option<TypeId>,
    layout: Layout,
    drop: DropFn,
    mutability: Mutability,
}

impl GlobalEventInfo {
    /// Gets the name of the event.
    ///
    /// This name is intended for debugging purposes and should not be relied
    /// upon for correctness.
    pub fn name(&self) -> &str {
        &self.name
    }

    /// Gets the ID of the event.
    pub fn id(&self) -> GlobalEventId {
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

    /// Gets the [mutability] of the event
    ///
    /// [mutability]: Event::Mutability
    pub fn mutability(&self) -> Mutability {
        self.mutability
    }
}

/// Lightweight identifier for a global event type.
///
/// Event identifiers are implemented using an [index] and a generation count.
/// The generation count ensures that IDs from despawned events are not reused
/// by new events.
///
/// An event identifier is only meaningful in the [`World`] it was created
/// from. Attempting to use an event ID in a different world will have
/// unexpected results.
///
/// [index]: GlobalEventIdx
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Default, Hash, Debug)]
pub struct GlobalEventId(Key);

impl GlobalEventId {
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
    pub const fn index(self) -> GlobalEventIdx {
        GlobalEventIdx(self.0.index())
    }

    /// Returns the generation count of this ID.
    pub const fn generation(self) -> u32 {
        self.0.generation().get()
    }
}

/// A [`GlobalEventId`] with the generation count stripped out.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct GlobalEventIdx(pub u32);

unsafe impl SparseIndex for GlobalEventIdx {
    const MAX: Self = Self(u32::MAX);

    fn index(self) -> usize {
        self.0.index()
    }

    fn from_index(idx: usize) -> Self {
        Self(u32::from_index(idx))
    }
}

/// An [`Event`] sent immediately after a new global event is added to the
/// world.
///
/// Contains the [`GlobalEventId`] of the added event.
#[derive(GlobalEvent, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct AddGlobalEvent(pub GlobalEventId);

/// An [`Event`] sent immediately before a global event is removed from the
/// world.
///
/// Contains the [`GlobalEventId`] of the event to be removed.
#[derive(GlobalEvent, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct RemoveGlobalEvent(pub GlobalEventId);
