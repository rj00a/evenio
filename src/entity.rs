use core::num::NonZeroU32;
use std::ops::Index;

use crate::archetype::{ArchetypeIdx, ArchetypeRow};
use crate::event::EventPtr;
use crate::prelude::World;
use crate::slot_map::{Key, NextKeyIter, SlotMap};
use crate::system::{Config, InitError, SystemInfo, SystemParam};
use crate::world::UnsafeWorldCell;

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

    pub(crate) fn get_mut(&mut self, id: EntityId) -> Option<&mut EntityLocation> {
        self.sm.get_mut(id.0)
    }

    pub fn get_by_index(&self, idx: EntityIdx) -> Option<EntityLocation> {
        self.sm.get_by_index(idx.0).map(|(_, v)| *v)
    }

    pub fn contains(&self, id: EntityId) -> bool {
        self.get(id).is_some()
    }

    fn add_with(&mut self, f: impl FnOnce(EntityId) -> EntityLocation) -> EntityId {
        if let Some(k) = self.sm.insert_with(|k| f(EntityId(k))) {
            EntityId(k)
        } else {
            panic!("too many entities")
        }
    }

    pub(crate) fn remove(&mut self, id: EntityId) -> Option<EntityLocation> {
        self.sm.remove(id.0)
    }

    pub fn len(&self) -> u32 {
        self.sm.len()
    }
}

impl Index<EntityId> for Entities {
    type Output = EntityLocation;

    fn index(&self, index: EntityId) -> &Self::Output {
        if let Some(loc) = self.sm.get(index.0) {
            loc
        } else {
            panic!("no such entity with ID of {index:?} exists")
        }
    }
}

impl Index<EntityIdx> for Entities {
    type Output = EntityLocation;

    fn index(&self, index: EntityIdx) -> &Self::Output {
        if let Some(loc) = self.sm.get_by_index(index.0).map(|(_, v)| v) {
            loc
        } else {
            panic!("no such entity with index of {index:?} exists")
        }
    }
}

impl SystemParam for &'_ Entities {
    type State = ();

    type Item<'a> = &'a Entities;

    fn init(_world: &mut World, _config: &mut Config) -> Result<Self::State, InitError> {
        Ok(())
    }

    unsafe fn get_param<'a>(
        _state: &'a mut Self::State,
        _system_info: &'a SystemInfo,
        _event_ptr: EventPtr<'a>,
        world: UnsafeWorldCell<'a>,
    ) -> Self::Item<'a> {
        world.entities()
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
        if let Some(k) = self.iter.next(&entities.sm) {
            self.count += 1;
            EntityId(k)
        } else {
            panic!("too many entities")
        }
    }

    pub(crate) fn flush(
        &mut self,
        entities: &mut Entities,
        mut f: impl FnMut(EntityId) -> EntityLocation,
    ) {
        for _ in 0..self.count {
            entities.add_with(&mut f);
        }

        self.count = 0;
        self.iter = entities.sm.next_key_iter();
    }
}
