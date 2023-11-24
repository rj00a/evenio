use std::fmt;
use std::num::NonZeroU32;

use crate::{archetype::{ArchetypeId, ArchetypeRow}, debug_checked::GetDebugChecked};

#[derive(Debug)]
pub struct Entities {
    metas: Vec<EntityMeta>,
    /// Indices into `metas` that are vacant.
    pending: Vec<u32>,
}

impl Entities {
    pub(crate) fn new() -> Self {
        Self {
            metas: vec![],
            pending: vec![],
        }
    }

    pub(crate) fn get(&self, id: EntityId) -> Option<&EntityLocation> {
        let meta = self.metas.get(id.index as usize)?;

        (meta.generation == id.generation).then_some(&meta.location)
    }

    pub(crate) fn get_mut(&mut self, id: EntityId) -> Option<&mut EntityLocation> {
        let meta = self.metas.get_mut(id.index as usize)?;

        (meta.generation == id.generation).then_some(&mut meta.location)
    }

    pub(crate) fn add(&mut self, f: impl FnOnce(EntityId) -> EntityLocation) -> EntityId {
        if let Some(idx) = self.pending.pop() {
            let meta = unsafe { self.metas.get_unchecked_mut(idx as usize) };

            let Some(next_gen) = NonZeroU32::new(meta.generation.get().wrapping_add(1)) else {
                // Retire this index permanently.
                return self.add(f);
            };

            let id = EntityId {
                index: idx,
                generation: next_gen,
            };

            let loc = f(id);
            debug_assert_ne!(loc.archetype, ArchetypeId::NULL);

            *meta = EntityMeta {
                generation: next_gen,
                location: loc,
            };

            id
        } else {
            // Make sure EntityId::NULL is always vacant.
            if self.metas.len() >= u32::MAX as usize {
                panic!("too many entities");
            }

            let id = EntityId {
                index: self.metas.len() as u32,
                generation: ONE,
            };

            let loc = f(id);
            debug_assert_ne!(loc.archetype, ArchetypeId::NULL);

            self.metas.push(EntityMeta {
                generation: ONE,
                location: loc,
            });

            id
        }
    }

    pub(crate) fn remove(&mut self, id: EntityId) -> Option<EntityLocation> {
        let meta = self.metas.get_mut(id.index as usize)?;

        if meta.generation == id.generation && meta.location.archetype != ArchetypeId::NULL {
            self.pending.push(id.index);
            let res = meta.location;
            meta.location.archetype = ArchetypeId::NULL;
            Some(res)
        } else {
            None
        }
    }

    pub(crate) fn len(&self) -> usize {
        // TODO: broken with retiring indices.
        self.metas.len() - self.pending.len()
    }

    pub(crate) fn reserve(&self, reserved: &mut ReservedEntities) -> EntityId {
        if reserved.cursor < self.pending.len() as u32 {
            let index = *unsafe {
                self.pending
                    .get_debug_checked(self.pending.len() - reserved.cursor as usize)
            };

            let meta = unsafe { self.metas.get_debug_checked(index as usize) };

            reserved.cursor += 1;

            let Some(next_gen) = meta.generation.checked_add(1) else {
                // Retired index. Cursor increases but no the count.
                return self.reserve(reserved);
            };

            reserved.count += 1;

            EntityId {
                index,
                generation: next_gen,
            }
        } else {
            let index = self.metas.len() as u32 + (reserved.cursor - self.pending.len() as u32);

            if index == EntityId::NULL.index {
                panic!("too many entities")
            }

            reserved.cursor += 1;
            reserved.count += 1;

            EntityId {
                index,
                generation: ONE,
            }
        }
    }

    pub(crate) fn flush_reserved(
        &mut self,
        reserved: &mut ReservedEntities,
        mut f: impl FnMut(EntityId) -> EntityLocation,
    ) {
        for _ in 0..reserved.count {
            self.add(&mut f);
        }

        reserved.cursor = 0;
        reserved.count = 0;
    }
}

const ONE: NonZeroU32 = match NonZeroU32::new(1) {
    Some(n) => n,
    None => unreachable!(),
};

#[derive(Debug)]
struct EntityMeta {
    generation: NonZeroU32,
    location: EntityLocation,
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub(crate) struct EntityLocation {
    /// The archetype that this entity is located in.
    pub(crate) archetype: ArchetypeId,
    /// The specific row within the archetype containing this entity.
    pub(crate) row: ArchetypeRow,
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

    pub const fn to_bits(self) -> u64 {
        (self.index as u64) << 32 | self.generation.get() as u64
    }

    pub const fn from_bits(bits: u64) -> Option<Self> {
        match NonZeroU32::new(bits as u32) {
            Some(gen) => Some(Self {
                index: (bits >> 32) as u32,
                generation: gen,
            }),
            None => None,
        }
    }
}

impl Default for EntityId {
    fn default() -> Self {
        Self::NULL
    }
}

impl fmt::Debug for EntityId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}v{}", self.index, self.generation)
    }
}

#[derive(Debug)]
pub(crate) struct ReservedEntities {
    cursor: u32,
    count: u32,
}

impl ReservedEntities {
    pub(crate) const fn new() -> Self {
        Self {
            cursor: 0,
            count: 0,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn entities_add_remove() {
        let mut entities = Entities::new();

        let l1 = EntityLocation {
            archetype: ArchetypeId(5),
            row: ArchetypeRow(5),
        };

        let e1 = entities.add(|_| l1);

        let l2 = EntityLocation {
            archetype: ArchetypeId(10),
            row: ArchetypeRow(10),
        };

        let e2 = entities.add(|_| l2);

        let l3 = EntityLocation {
            archetype: ArchetypeId(15),
            row: ArchetypeRow(15),
        };

        let e3 = entities.add(|_| l3);

        assert_eq!(entities.get(e1), Some(&l1));
        assert_eq!(entities.get(e2), Some(&l2));
        assert_eq!(entities.remove(e2), Some(l2));
        assert_eq!(entities.remove(e2), None);
        assert_eq!(entities.get(e1), Some(&l1));
        assert_eq!(entities.get(e3), Some(&l3));
    }

    #[test]
    fn entitites_reserve() {
        let mut entities = Entities::new();
        let mut reserved = ReservedEntities::new();

        let e1 = entities.reserve(&mut reserved);
        let e2 = entities.reserve(&mut reserved);
        let e3 = entities.reserve(&mut reserved);

        assert_eq!(entities.get(e1), None);
        assert!(e1 != e2 && e2 != e3 && e1 != e3);

        let l = |i| EntityLocation {
            archetype: ArchetypeId(i),
            row: ArchetypeRow(i),
        };

        entities.flush_reserved(&mut reserved, |id| l(id.index + 1));

        assert_eq!(entities.get(e1), Some(&l(1)));
        assert_eq!(entities.get(e2), Some(&l(2)));
        assert_eq!(entities.get(e3), Some(&l(3)));

        // TODO: test retiring.
    }
}
