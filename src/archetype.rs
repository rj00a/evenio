use std::ptr::NonNull;

use ahash::HashMap;

use crate::component::{ComponentId, Components};
use crate::entity::EntityId;
use crate::erased_vec::ErasedVec;
use crate::util::{GetDebugChecked, UnwrapDebugChecked};

#[derive(Debug)]
pub(crate) struct Archetypes {
    archetypes: Vec<Archetype>,
    by_components: HashMap<Box<[ComponentId]>, ArchetypeId>,
}

impl Archetypes {
    pub(crate) fn new() -> Self {
        Self {
            archetypes: vec![Archetype::empty()],
            by_components: HashMap::default(),
        }
    }

    pub(crate) fn empty_mut(&mut self) -> &mut Archetype {
        unsafe { self.archetypes.get_debug_checked_mut(0) }
    }

    /*
    pub(crate) unsafe fn get_or_init(
        &mut self,
        comp: ComponentId,
        comps: &ComponentRegistry,
    ) -> &mut Archetype {
        match self.by_components.get([comp].as_slice()) {
            Some(&id) => unsafe { self.archetypes.get_debug_checked_mut(id.0.get() as usize) },
            None => {
                if self.archetypes.len() >= u32::MAX as usize {
                    panic!("too many archetypes");
                }

                let id = ArchetypeId(self.archetypes.len() as u32);

                let arch = Archetype::new([comp], comps);
                self.archetypes.push(arch);

                self.by_components.insert([comp].into(), id);

                unsafe { self.archetypes.get_debug_checked_mut(id.0 as usize) }
            }
        }
    }*/
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub(crate) struct ArchetypeId(pub(crate) u32);

impl ArchetypeId {
    /// ID of the archetype with no components.
    pub const EMPTY: Self = Self(0);
    /// Marker used for nonexistent entities.
    pub const NULL: Self = Self(u32::MAX);
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub(crate) struct ArchetypeRow(pub(crate) u32);

#[derive(Debug)]
pub(crate) struct Archetype {
    /// Entity IDs of the entities in this archetype.
    ids: Vec<EntityId>,
    /// Columns of component data for this archetype. Sorted by
    /// [`Column::component_id`].
    columns: Box<[Column]>,
}

impl Archetype {
    fn empty() -> Self {
        Self {
            ids: vec![],
            columns: Box::new([]),
        }
    }

    pub(crate) fn new(ids: impl IntoIterator<Item = ComponentId>, comps: &Components) -> Self {
        Self {
            ids: vec![],
            columns: ids
                .into_iter()
                .map(|id| {
                    let comp =
                        unsafe { comps.get(id).expect_debug_checked("invalid component ID") };

                    Column {
                        data: unsafe { ErasedVec::new(comp.layout(), comp.drop()) },
                        component_id: id,
                    }
                })
                .collect(),
        }
    }

    /// Add an entity to this archetype.
    pub(crate) unsafe fn add(
        &mut self,
        id: EntityId,
    ) -> (ArchetypeRow, impl Iterator<Item = NonNull<u8>> + '_) {
        debug_assert!(self.ids.len() <= u32::MAX as usize);

        let row = ArchetypeRow(self.ids.len() as u32);
        self.ids.push(id);

        let iter = self.columns.iter_mut().map(|col| col.data.push());

        (row, iter)
    }

    pub(crate) unsafe fn swap_remove(&mut self, row: ArchetypeRow) -> Option<EntityId> {
        debug_assert!(row.0 < self.ids.len() as u32);

        for col in self.columns.iter_mut() {
            col.data.swap_remove(row.0 as usize);
        }

        self.ids.swap_remove(row.0 as usize);
        self.ids.get(row.0 as usize).copied()
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

pub(crate) struct ArchetypeSwapRemoveResult {
    /// ID of the entity that was moved to fill the space left by the removed
    /// entity.
    pub(crate) moved_entity: EntityId,
    /// Where the moved entity ended up.
    pub(crate) dest_row: ArchetypeRow,
}

#[cfg(test)]
mod tests {
    use super::*;
}
