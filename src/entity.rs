//! Entity related items.

use core::ops::Index;

use crate::archetype::{ArchetypeIdx, ArchetypeRow};
use crate::event::EventPtr;
use crate::handler::{HandlerConfig, HandlerInfo, HandlerParam, InitError};
use crate::prelude::World;
use crate::slot_map::{Key, NextKeyIter, SlotMap};
use crate::world::UnsafeWorldCell;

/// Contains metadata for all the entities in a world.
///
/// This can be obtained in a handler by using the `&Entities` handler
/// parameter.
///
/// ```
/// # use evenio::prelude::*;
/// # use evenio::entity::Entities;
/// #
/// # #[derive(Event)] struct E;
/// #
/// # let mut world = World::new();
/// world.add_handler(|_: Receiver<E>, entities: &Entities| {});
/// ```
#[derive(Debug)]
pub struct Entities {
    locs: SlotMap<EntityLocation>,
}

impl Entities {
    pub(crate) fn new() -> Self {
        Self {
            locs: SlotMap::new(),
        }
    }

    /// Gets the [`EntityLocation`] of the given entity. Returns `None` if the
    /// ID is invalid.
    pub fn get(&self, id: EntityId) -> Option<EntityLocation> {
        self.locs.get(id.0).copied()
    }

    pub(crate) fn get_mut(&mut self, id: EntityId) -> Option<&mut EntityLocation> {
        self.locs.get_mut(id.0)
    }

    /// Gets the [`EntityLocation`] of an entity using its [`EntityIdx`].
    /// Returns `None` if the index is invalid.
    pub fn get_by_index(&self, idx: EntityIdx) -> Option<EntityLocation> {
        self.locs.get_by_index(idx.0).map(|(_, v)| *v)
    }

    /// Does the given entity exist in the world?
    pub fn contains(&self, id: EntityId) -> bool {
        self.get(id).is_some()
    }

    fn add_with(&mut self, f: impl FnOnce(EntityId) -> EntityLocation) -> EntityId {
        if let Some(k) = self.locs.insert_with(|k| f(EntityId(k))) {
            EntityId(k)
        } else {
            panic!("too many entities")
        }
    }

    pub(crate) fn remove(&mut self, id: EntityId) -> Option<EntityLocation> {
        self.locs.remove(id.0)
    }

    /// Returns the total number of entities.
    pub fn len(&self) -> u32 {
        self.locs.len()
    }

    /// Returns an iterator over all entity locations.
    pub fn iter(&self) -> impl Iterator<Item = EntityLocation> + '_ {
        self.locs.iter().map(|(_, v)| *v)
    }
}

impl Index<EntityId> for Entities {
    type Output = EntityLocation;

    /// Panics if the ID is invalid.
    fn index(&self, id: EntityId) -> &Self::Output {
        if let Some(loc) = self.locs.get(id.0) {
            loc
        } else {
            panic!("no such entity with ID of {id:?} exists")
        }
    }
}

impl Index<EntityIdx> for Entities {
    type Output = EntityLocation;

    /// Panics if the index is invalid.
    fn index(&self, idx: EntityIdx) -> &Self::Output {
        if let Some(loc) = self.locs.get_by_index(idx.0).map(|(_, v)| v) {
            loc
        } else {
            panic!("no such entity with index of {idx:?} exists")
        }
    }
}

unsafe impl HandlerParam for &'_ Entities {
    type State = ();

    type This<'a> = &'a Entities;

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
        world.entities()
    }

    fn refresh_archetype(_state: &mut Self::State, _arch: &crate::archetype::Archetype) {}

    fn remove_archetype(_state: &mut Self::State, _arch: &crate::archetype::Archetype) {}
}

/// The location of an entity in an archetype.
#[derive(Copy, Clone, Debug)]
pub struct EntityLocation {
    /// The archetype where the entity is located.
    pub archetype: ArchetypeIdx,
    /// The specific row in the archetype where the entity is located.
    pub row: ArchetypeRow,
}

impl EntityLocation {
    /// A location which is always invalid.
    pub(crate) const NULL: Self = Self {
        archetype: ArchetypeIdx::NULL,
        row: ArchetypeRow::NULL,
    };
}

/// Lightweight identifier for an entity.
///
/// Entity identifiers are implemented using an [index] and a generation count.
/// The generation count ensures that IDs from despawned entities are not reused
/// by new entities.
///
/// An entity identifier is only meaningful in the [`World`] it was created
/// from. Attempting to use an entity ID in a different world will have
/// unexpected results.
///
/// [index]: EntityIdx
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Default, Hash, Debug)]
pub struct EntityId(Key);

impl EntityId {
    /// The entity ID which never identifies a live entity. This is the default
    /// value for `EntityId`.
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
    pub const fn index(self) -> EntityIdx {
        EntityIdx(self.0.index())
    }

    /// Returns the generation count of this ID.
    pub const fn generation(self) -> u32 {
        self.0.generation().get()
    }
}

/// An [`EntityId`] with the generation count stripped out.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Default, Hash, Debug)]
pub struct EntityIdx(pub u32);

/// A queue of entities to be spawned into the world.
#[derive(Debug)]
pub(crate) struct ReservedEntities {
    iter: NextKeyIter<EntityLocation>,
    count: u32,
}

impl ReservedEntities {
    pub(crate) fn new() -> Self {
        Self {
            iter: NextKeyIter::new(),
            count: 0,
        }
    }

    pub(crate) fn reserve(&mut self, entities: &Entities) -> EntityId {
        if let Some(k) = self.iter.next(&entities.locs) {
            self.count += 1;
            EntityId(k)
        } else {
            panic!("too many entities")
        }
    }

    pub(crate) fn spawn_all(
        &mut self,
        entities: &mut Entities,
        mut f: impl FnMut(EntityId) -> EntityLocation,
    ) {
        for _ in 0..self.count {
            entities.add_with(&mut f);
        }

        self.iter = entities.locs.next_key_iter();
        self.count = 0;
    }

    pub(crate) fn refresh(&mut self, entities: &Entities) {
        debug_assert_eq!(self.count, 0);
        self.iter = entities.locs.next_key_iter();
    }
}

#[cfg(test)]
mod tests {
    use crate::entity::Entities;
    use crate::prelude::*;

    #[test]
    fn spawn_despawn_entity() {
        let mut world = World::new();

        let e1 = world.spawn();
        assert!(world.entities().contains(e1));
        world.despawn(e1);
        assert!(!world.entities().contains(e1));

        let e2 = world.spawn();
        assert!(world.entities().contains(e2));
        assert!(!world.entities().contains(e1));
        assert_ne!(e1, e2);
        world.despawn(e2);
        assert!(!world.entities().contains(e2));
    }

    #[test]
    fn spawn_despawn_queued() {
        let mut world = World::new();

        #[derive(Event)]
        struct E1;

        #[derive(Event)]
        struct E2 {
            a: EntityId,
            b: EntityId,
        }

        world.add_handler(|_: Receiver<E1>, mut s: Sender<(Despawn, E2)>| {
            let a = s.spawn();
            let b = s.spawn();
            s.despawn(b);
            s.send(E2 { a, b });
        });

        world.add_handler(|r: Receiver<E2>, mut s: Sender<()>| {
            let c = s.spawn();
            assert_ne!(r.event.a, c);
            assert_ne!(r.event.b, c);
        });
    }

    #[test]
    fn spawn_event_entity_exists() {
        let mut world = World::new();

        #[derive(Event)]
        struct E;

        world.add_handler(|r: Receiver<Spawn, ()>, entities: &Entities| {
            assert!(entities.contains(r.event.0));
        });
    }
}
