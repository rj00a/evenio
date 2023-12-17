use alloc::collections::BTreeMap;
use core::ptr::NonNull;
use std::collections::BinaryHeap;

use slab::Slab;

use crate::blob_vec::BlobVec;
use crate::component::ComponentIdx;
use crate::debug_checked::{GetDebugChecked, UnwrapDebugChecked};
use crate::entity::EntityId;

#[derive(Debug)]
pub struct Archetypes {
    archetypes: Slab<Archetype>,
    by_components: BTreeMap<Box<[ComponentIdx]>, ArchetypeIdx>,
    indices: BinaryHeap<ArchetypeIdx>,
}

impl Archetypes {
    pub(crate) fn new() -> Self {
        Self {
            archetypes: Slab::from_iter([(0, Archetype::empty())]),
            by_components: BTreeMap::from_iter([(vec![].into_boxed_slice(), ArchetypeIdx::EMPTY)]),
            indices: BinaryHeap::from_iter([ArchetypeIdx::EMPTY]),
        }
    }

    pub fn empty(&self) -> &Archetype {
        // SAFETY: The empty archetype is always at index 0.
        unsafe { self.archetypes.get_debug_checked(0) }
    }

    pub(crate) fn empty_mut(&mut self) -> &mut Archetype {
        // SAFETY: The empty archetype is always at index 0.
        unsafe { self.archetypes.get_debug_checked_mut(0) }
    }

    pub fn get(&self, idx: ArchetypeIdx) -> Option<&Archetype> {
        self.archetypes.get(idx.0 as usize)
    }

    pub fn by_components(&self, components: &[ComponentIdx]) -> Option<&Archetype> {
        let idx = *self.by_components.get(components)?;
        Some(unsafe { self.get(idx).unwrap_debug_checked() })
    }

    pub fn max_archetype_index(&self) -> ArchetypeIdx {
        // SAFETY: `indices` is nonempty because the empty archetype is always present.
        *unsafe { self.indices.peek().unwrap_debug_checked() }
    }

    pub fn iter(&self) -> impl Iterator<Item = &Archetype> {
        self.archetypes.iter().map(|(_, v)| v)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct ArchetypeIdx(pub u32);

impl ArchetypeIdx {
    /// Index of the archetype with no components.
    pub const EMPTY: Self = Self(0);
    /// The archetype index that is always invalid.
    pub const NULL: Self = Self(u32::MAX);
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct ArchetypeRow(pub u32);

#[derive(Debug)]
pub struct Archetype {
    /// The index of this archetype. Provided here for convenience.
    index: ArchetypeIdx,
    /// Entity IDs of the entities in this archetype.
    entity_ids: Vec<EntityId>,
    /// Columns of component data in this archetype. Sorted by component index.
    columns: Box<[Column]>,
    insert_components: BTreeMap<ComponentIdx, ArchetypeIdx>,
    remove_components: BTreeMap<ComponentIdx, ArchetypeIdx>,
}

impl Archetype {
    fn empty() -> Self {
        Self {
            index: ArchetypeIdx::EMPTY,
            entity_ids: vec![],
            columns: Box::new([]),
            insert_components: BTreeMap::new(),
            remove_components: BTreeMap::new(),
        }
    }

    pub fn index(&self) -> ArchetypeIdx {
        self.index
    }

    pub fn entity_count(&self) -> u32 {
        self.entity_ids.len() as u32
    }
}

#[derive(Debug)]
pub struct Column {
    /// Component data in this column.
    data: BlobVec,
    /// Type of data in this column.
    component_idx: ComponentIdx,
}

impl Column {
    pub fn data(&self) -> NonNull<u8> {
        self.data.as_ptr()
    }

    pub fn component_index(&self) -> ComponentIdx {
        self.component_idx
    }
}

// SAFETY: Components are guaranteed `Send` and `Sync`.
unsafe impl Send for Column {}
unsafe impl Sync for Column {}
