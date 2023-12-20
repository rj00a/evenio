use core::num::NonZeroU32;

use crate::archetype::{ArchetypeIdx, ArchetypeRow};
use crate::slot_map::{Key, SlotMap};

#[derive(Debug)]
pub struct Entities {
    sm: SlotMap<EntityLocation>,
}

impl Entities {
    pub(crate) fn new() -> Self {
        Self { sm: SlotMap::new() }
    }

    pub fn get(&self, id: EntityId) -> Option<EntityLocation> {
        self.sm.get(id.0).copied()
    }

    pub(crate) fn insert(&mut self, loc: EntityLocation) -> EntityId {
        EntityId(self.sm.insert(loc))
    }

    pub(crate) fn remove(&mut self, id: EntityId) -> Option<EntityLocation> {
        self.sm.remove(id.0)
    }
}

#[derive(Copy, Clone, Debug)]
pub struct EntityLocation {
    pub archetype: ArchetypeIdx,
    pub row: ArchetypeRow,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Default, Hash, Debug)]
pub struct EntityId(Key);

impl EntityId {
    pub const NULL: Self = Self(Key::NULL);

    pub const fn new(index: u32, generation: NonZeroU32) -> Option<Self> {
        match Key::new(index, generation) {
            Some(k) => Some(Self(k)),
            None => None,
        }
    }

    pub const fn index(self) -> EntityIdx {
        EntityIdx(self.0.index())
    }

    pub const fn generation(self) -> u32 {
        self.0.generation().get()
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Default, Hash, Debug)]
pub struct EntityIdx(pub u32);

#[derive(Debug)]
pub(crate) struct ReservedEntities {
    // TODO
}

impl ReservedEntities {
    pub(crate) const fn new() -> Self {
        Self {}
    }

    pub(crate) fn reserve(&mut self, entities: &Entities) -> EntityId {
        todo!()
    }
}
