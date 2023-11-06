use std::fmt;

use crate::archetype::ArchetypeId;
use crate::id::{Ident, Storage};

#[derive(Debug)]
pub(crate) struct Entities {
    locations: Storage<EntityLocation>,
}

impl Entities {
    pub(crate) fn new() -> Self {
        Self {
            locations: Storage::new(),
        }
    }

    pub(crate) fn add(&mut self, loc: EntityLocation) -> EntityId {
        EntityId(self.locations.add(loc))
    }

    pub(crate) fn remove(&mut self, id: EntityId) -> Option<EntityLocation> {
        self.locations.remove(id.0)
    }
}

#[derive(Debug)]
pub(crate) struct EntityLocation {
    /// The archetype that this entity is located in.
    pub(crate) archetype_id: ArchetypeId,
    /// The specific row within the archetype containing this entity.
    pub(crate) row: u32,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default, Debug)]
pub struct EntityId(Ident);

impl EntityId {
    pub const NULL: Self = Self(Ident::NULL);

    pub fn to_bits(self) -> u64 {
        self.0.to_bits()
    }

    pub fn from_bits(bits: u64) -> Option<Self> {
        Ident::from_bits(bits).map(Self)
    }
}
