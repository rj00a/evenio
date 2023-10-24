use std::alloc::{self, Layout};
use std::mem;
use std::ptr::{self, NonNull};

use crate::component::{ComponentId, Components};
use crate::entity::EntityId;
use crate::erased_vec::ErasedVec;

#[derive(Debug)]
pub(crate) struct Archetypes {
    archetypes: Vec<Archetype>,
}

impl Archetypes {
    pub(crate) fn new() -> Self {
        Self { archetypes: vec![] }
    }
}

#[derive(Debug)]
pub(crate) struct Archetype {
    /// Entity IDs of the entities in this archetype.
    ids: Vec<EntityId>,
    /// Columns of component data for this archetype.
    columns: Box<[Column]>,
}

impl Archetype {
    pub(crate) fn new(ids: impl IntoIterator<Item = ComponentId>, comps: &Components) -> Self {
        Self {
            ids: vec![],
            columns: ids
                .into_iter()
                .map(|id| {
                    let comp = comps.get(id).expect("invalid component ID");

                    Column {
                        data: unsafe { ErasedVec::new(comp.layout(), comp.drop()) },
                        component_id: id,
                    }
                })
                .collect(),
        }
    }

    /// Allocates space for one entity and returns its index in this archetype.
    ///
    /// The component data will be uninitialized, and should be initialized
    /// immediately after this is called.
    ///
    /// `id` is the entity ID assigned to the new entity.
    ///
    /// # Safety
    ///
    /// - `comps` must contain the correct components for this archetype.
    pub(crate) unsafe fn alloc(&mut self, id: EntityId, comps: &Components) -> u32 {
        todo!()
    }

    /// Removes an entity from the archetype. All components are dropped. The
    /// removed entity is replaced by the last entity.
    ///
    /// The entity ID of the last entity prior to the move is returned.
    ///
    /// # Safety
    ///
    /// - `index` must be in bounds.
    /// - `comps` must contain the correct components for this archetype.
    pub(crate) unsafe fn remove(&mut self, index: u32, comps: &Components) -> Option<EntityId> {
        todo!()
    }

    ///
    /// # Safety
    ///
    /// - All components must be initialized.
    pub(crate) unsafe fn drop(self, comps: &Components) {
        todo!()
    }

    /// Returns the number of entities in this archetype.
    pub(crate) fn len(&mut self) -> u32 {
        self.ids.len() as u32
    }
}

#[derive(Debug)]
struct Column {
    /// Component data in the column.
    data: ErasedVec,
    /// The type of data in this column.
    component_id: ComponentId,
}

// SAFETY: Component types are guaranteed Send and Sync.
unsafe impl Send for Column {}
unsafe impl Sync for Column {}

#[cfg(test)]
mod tests {
    use super::*;
}
