use std::cmp::Ordering;
use std::collections::{btree_map, hash_map, BTreeMap, BTreeSet};
use std::ptr::{self, NonNull};

use ahash::HashMap;
use slab::Slab;

use crate::component::{ComponentId, Components};
use crate::debug_checked::{GetDebugChecked, UnwrapDebugChecked};
use crate::entity::{Entities, EntityId, EntityLocation};
use crate::erased_vec::ErasedVec;
use crate::system::System;

#[derive(Debug)]
pub struct Archetypes {
    archetypes: Slab<Archetype>,
    by_components: HashMap<Box<[ComponentId]>, ArchetypeId>,
}

impl Archetypes {
    pub(crate) fn new() -> Self {
        Self {
            archetypes: Slab::from_iter([(0, Archetype::empty())]),
            by_components: HashMap::default(),
        }
    }

    pub(crate) fn empty_mut(&mut self) -> &mut Archetype {
        // SAFETY: The empty archetype is always at index 0.
        unsafe { self.archetypes.get_debug_checked_mut(0) }
    }

    pub(crate) fn by_components(&self, components: &[ComponentId]) -> Option<&Archetype> {
        todo!()
    }

    pub(crate) fn get(&self, id: ArchetypeId) -> Option<&Archetype> {
        self.archetypes.get(id.0 as usize)
    }

    pub(crate) fn get_mut(&mut self, id: ArchetypeId) -> Option<&mut Archetype> {
        self.archetypes.get_mut(id.0 as usize)
    }

    pub(crate) unsafe fn get_debug_checked(&self, id: ArchetypeId) -> &Archetype {
        self.archetypes.get_debug_checked(id.0 as usize)
    }

    pub(crate) unsafe fn get_debug_checked_mut(&mut self, id: ArchetypeId) -> &mut Archetype {
        self.archetypes.get_debug_checked_mut(id.0 as usize)
    }

    pub fn iter(&self) -> impl DoubleEndedIterator<Item = (ArchetypeId, &Archetype)> + '_ {
        self.archetypes
            .iter()
            .map(|(k, v)| (ArchetypeId(k as u32), v))
    }

    /// Traverses one edge of the archetype graph.
    pub(crate) unsafe fn traverse_insert(
        &mut self,
        current_arch_id: ArchetypeId,
        component_id: ComponentId,
        components: &mut Components,
    ) -> ArchetypeId {
        let next_arch_idx = self.archetypes.vacant_key();

        let current_arch = unsafe {
            self.archetypes
                .get_debug_checked_mut(current_arch_id.0 as usize)
        };

        match current_arch.insert_components.entry(component_id) {
            btree_map::Entry::Vacant(vacant_insert_components) => {
                let Err(idx) = current_arch
                    .columns
                    .binary_search_by_key(&component_id, |c| c.component_id)
                else {
                    // If this archetype already has the component, then stop here.
                    return current_arch_id;
                };

                let mut new_components = Vec::with_capacity(current_arch.columns.len() + 1);
                for (i, c) in current_arch.columns.iter().enumerate() {
                    if i == idx {
                        new_components.push(component_id);
                    }

                    new_components.push(c.component_id);
                }

                match self.by_components.entry(new_components.into_boxed_slice()) {
                    hash_map::Entry::Occupied(o) => *vacant_insert_components.insert(*o.get()),
                    hash_map::Entry::Vacant(vacant_by_components) => {
                        if next_arch_idx >= u32::MAX as usize {
                            panic!("too many archetypes");
                        }

                        let arch_id = ArchetypeId(next_arch_idx as u32);

                        let mut new_arch =
                            Archetype::new(vacant_by_components.key().iter().copied(), components);

                        new_arch
                            .remove_components
                            .insert(component_id, current_arch_id);

                        for &component_id in vacant_by_components.key().iter() {
                            let info = components.get_debug_checked_mut(component_id);

                            assert!(info.member_of.insert(arch_id));
                        }

                        vacant_by_components.insert(arch_id);

                        vacant_insert_components.insert(arch_id);

                        self.archetypes.insert(new_arch);

                        arch_id
                    }
                }
            }
            btree_map::Entry::Occupied(o) => *o.get(),
        }
    }

    pub(crate) unsafe fn traverse_remove(
        &mut self,
        current_arch_id: ArchetypeId,
        component_id: ComponentId,
        components: &Components,
    ) -> ArchetypeId {
        let next_arch_idx = self.archetypes.vacant_key();

        let current_arch = unsafe {
            self.archetypes
                .get_debug_checked_mut(current_arch_id.0 as usize)
        };

        match current_arch.remove_components.entry(component_id) {
            btree_map::Entry::Vacant(vacant_remove_components) => {
                if current_arch
                    .columns
                    .binary_search_by_key(&component_id, |c| c.component_id)
                    .is_err()
                {
                    return current_arch_id;
                }

                let mut new_components = Vec::with_capacity(current_arch.columns.len() - 1);
                new_components.extend(
                    current_arch
                        .columns
                        .iter()
                        .map(|c| c.component_id)
                        .filter(|&c| c != component_id),
                );

                match self.by_components.entry(new_components.into_boxed_slice()) {
                    hash_map::Entry::Occupied(o) => *vacant_remove_components.insert(*o.get()),
                    hash_map::Entry::Vacant(vacant_by_components) => {
                        if next_arch_idx >= u32::MAX as usize {
                            panic!("too many archetypes");
                        }

                        let mut new_arch =
                            Archetype::new(vacant_by_components.key().iter().copied(), components);

                        new_arch
                            .insert_components
                            .insert(component_id, current_arch_id);

                        let arch_id = ArchetypeId(next_arch_idx as u32);

                        vacant_by_components.insert(arch_id);

                        vacant_remove_components.insert(arch_id);

                        self.archetypes.insert(new_arch);

                        arch_id
                    }
                }
            }
            btree_map::Entry::Occupied(o) => *o.get(),
        }
    }

    pub(crate) unsafe fn move_entity(
        &mut self,
        src_row: ArchetypeRow,
        src: ArchetypeId,
        dst: ArchetypeId,
        new_components: impl IntoIterator<Item = (ComponentId, *mut u8)>,
        entities: &mut Entities,
    ) -> ArchetypeRow {
        if src == dst {
            todo!("do something for this case");
        }

        let (src_arch, dst_arch) = self
            .archetypes
            .get2_mut(src.0 as usize, dst.0 as usize)
            .unwrap();

        let dst_row = ArchetypeRow(dst_arch.len());

        let mut src_it = src_arch.columns.iter_mut().peekable();
        let mut dst_it = dst_arch.columns.iter_mut().peekable();

        let mut new_components = new_components.into_iter();

        // TODO: does this optimize better with raw pointers?
        loop {
            match (src_it.peek_mut(), dst_it.peek_mut()) {
                (None, None) => break,
                (None, Some(dst_col)) => {
                    let (component_id, component_ptr) =
                        new_components.next().unwrap_debug_checked();

                    debug_assert_eq!(component_id, dst_col.component_id);

                    ptr::copy_nonoverlapping(
                        component_ptr,
                        dst_col.data.push().as_ptr(),
                        dst_col.data.elem_layout().size(),
                    );

                    dst_it.next();
                }
                (Some(src_col), None) => {
                    src_col.data.swap_remove(src_row.0 as usize);
                }
                (Some(src_col), Some(dst_col)) => {
                    match src_col.component_id.cmp(&dst_col.component_id) {
                        Ordering::Less => {
                            src_col.data.swap_remove(src_row.0 as usize);
                            src_it.next();
                        }
                        Ordering::Equal => {
                            src_col
                                .data
                                .transfer_elem(&mut dst_col.data, src_row.0 as usize);
                            src_it.next();
                            dst_it.next();
                        }
                        Ordering::Greater => {
                            let (component_id, component_ptr) =
                                new_components.next().unwrap_debug_checked();

                            debug_assert_eq!(component_id, dst_col.component_id);

                            ptr::copy_nonoverlapping(
                                component_ptr,
                                dst_col.data.push().as_ptr(),
                                dst_col.data.elem_layout().size(),
                            );

                            dst_it.next();
                        }
                    }
                }
            }
        }

        debug_assert!(new_components.next().is_none());

        let entity_id = src_arch.entity_ids.swap_remove(src_row.0 as usize);
        dst_arch.entity_ids.push(entity_id);

        unsafe { entities.get_mut(entity_id).unwrap_debug_checked() }.row = dst_row;

        if let Some(&swapped_entity_id) = src_arch.entity_ids.get(src_row.0 as usize) {
            unsafe { entities.get_mut(swapped_entity_id).unwrap_debug_checked() }.row = src_row;
        }

        if src_arch.entity_ids.is_empty() {
            // TODO: mark archetype as inactive?
        }

        dst_row
    }
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
pub struct Archetype {
    /// Entity IDs of the entities in this archetype.
    entity_ids: Vec<EntityId>,
    /// Columns of component data for this archetype. Sorted by
    /// [`Column::component_id`].
    columns: Box<[Column]>,
    insert_components: BTreeMap<ComponentId, ArchetypeId>,
    remove_components: BTreeMap<ComponentId, ArchetypeId>,
    /// Systems that need to be informed about changes to this archetype.
    subscribers: BTreeSet<*mut dyn System>,
}

impl Archetype {
    fn empty() -> Self {
        Self {
            entity_ids: vec![],
            columns: Box::new([]),
            insert_components: BTreeMap::new(),
            remove_components: BTreeMap::new(),
            subscribers: BTreeSet::new(),
        }
    }

    /// # Safety
    ///
    /// Iterator must be sorted and all IDs must be valid.
    unsafe fn new(ids: impl IntoIterator<Item = ComponentId>, comps: &Components) -> Self {
        Self {
            entity_ids: vec![],
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
            insert_components: BTreeMap::new(),
            remove_components: BTreeMap::new(),
            subscribers: BTreeSet::new(),
        }
    }

    /// Add an entity to this archetype.
    pub(crate) unsafe fn add(
        &mut self,
        id: EntityId,
    ) -> (ArchetypeRow, impl Iterator<Item = NonNull<u8>> + '_) {
        debug_assert!(self.entity_ids.len() <= u32::MAX as usize);

        let row = ArchetypeRow(self.entity_ids.len() as u32);
        self.entity_ids.push(id);

        let iter = self.columns.iter_mut().map(|col| col.data.push());

        (row, iter)
    }

    pub(crate) unsafe fn swap_remove(&mut self, row: ArchetypeRow) -> Option<EntityId> {
        debug_assert!(row.0 < self.entity_ids.len() as u32);

        for col in self.columns.iter_mut() {
            col.data.swap_remove(row.0 as usize);
        }

        self.entity_ids.swap_remove(row.0 as usize);
        self.entity_ids.get(row.0 as usize).copied()
    }

    pub fn columns(&self) -> &[Column] {
        &self.columns
    }

    pub fn entity_ids(&self) -> NonNull<EntityId> {
        unsafe { NonNull::new(self.entity_ids.as_ptr().cast_mut()).unwrap_debug_checked() }
    }

    /// Returns the number of entities in this archetype.
    pub(crate) fn len(&self) -> u32 {
        self.entity_ids.len() as u32
    }

    pub unsafe fn subscribe(&mut self, system: *mut dyn System) {
        self.subscribers.insert(system);
    }
}

#[derive(Debug)]
pub struct Column {
    /// Component data in the column.
    data: ErasedVec,
    /// The type of data in this column.
    component_id: ComponentId,
}

impl Column {
    pub fn component_id(&self) -> ComponentId {
        self.component_id
    }

    pub fn data(&self) -> NonNull<u8> {
        self.data.as_ptr()
    }
}

/*
// SAFETY: Component types are guaranteed `Send` and `Sync`.
unsafe impl Send for Column {}
unsafe impl Sync for Column {}
*/

/*
pub(crate) struct ArchetypeSwapRemoveResult {
    /// ID of the entity that was moved to fill the space left by the removed
    /// entity.
    pub(crate) moved_entity: EntityId,
    /// Where the moved entity ended up.
    pub(crate) dest_row: ArchetypeRow,
}*/

#[cfg(test)]
mod tests {
    use super::*;
}
