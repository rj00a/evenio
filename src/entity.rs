use std::fmt;
use std::num::NonZeroU32;

pub(crate) struct Entities {
    entities: Vec<EntityLocation>,
    /// Indices of slots in `entities` that are vacant.
    freelist: Vec<u32>,
}

pub(crate) struct EntityLocation {
    /// Generation of this entity.
    pub(crate) generation: NonZeroU32,
    /// The archetype that this entity is located in.
    pub(crate) archetype_index: u32,
    /// The specific row within the archetype containing this entity.
    pub(crate) row: u32,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct EntityId {
    index: u32,
    generation: NonZeroU32,
}

impl EntityId {
    pub const NULL: Self = Self {
        index: u32::MAX,
        generation: NonZeroU32::MAX,
    };
}

impl Default for EntityId {
    fn default() -> Self {
        Self::NULL
    }
}

impl fmt::Debug for EntityId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}v{}", self.index, self.generation.get())
    }
}
