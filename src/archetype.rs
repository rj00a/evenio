use alloc::collections::BTreeMap;
use core::ptr::NonNull;
use std::collections::btree_map::Entry;
use std::collections::BTreeSet;

use slab::Slab;

use crate::blob_vec::BlobVec;
use crate::component::{ComponentIdx, Components};
use crate::debug_checked::{GetDebugChecked, UnwrapDebugChecked};
use crate::entity::EntityId;
use crate::event::{EntityEventIdx, EventIdx};
use crate::sparse::SparseIndex;
use crate::sparse_map::SparseMap;
use crate::system::{RefreshArchetypeReason, SystemInfo, SystemInfoPtr, SystemList, Systems};

#[derive(Debug)]
pub struct Archetypes {
    archetypes: Slab<Archetype>,
    by_components: BTreeMap<Box<[ComponentIdx]>, ArchetypeIdx>,
    // indices: BinaryHeap<ArchetypeIdx>,
}

impl Archetypes {
    pub(crate) fn new() -> Self {
        Self {
            archetypes: Slab::from_iter([(0, Archetype::empty())]),
            by_components: BTreeMap::from_iter([(vec![].into_boxed_slice(), ArchetypeIdx::EMPTY)]),
            // indices: BinaryHeap::from_iter([ArchetypeIdx::EMPTY]),
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

    /*
    pub fn max_archetype_index(&self) -> ArchetypeIdx {
        // SAFETY: `indices` is nonempty because the empty archetype is always present.
        *unsafe { self.indices.peek().unwrap_debug_checked() }
    }
    */

    pub fn iter(&self) -> impl Iterator<Item = &Archetype> {
        self.archetypes.iter().map(|(_, v)| v)
    }

    /*
    pub(crate) fn iter_mut(&mut self) -> impl Iterator<Item = &mut Archetype> {
        self.archetypes.iter_mut().map(|(_, v)| v)
    }
    */

    pub(crate) fn register_system(&mut self, info: &mut SystemInfo) {
        // TODO: use a `Component -> Vec<Archetype>` index to make this faster?
        for (_, arch) in self.archetypes.iter_mut() {
            arch.register_system(info);
        }
    }

    /// Traverses one edge of the archetype graph in the insertion direction.
    /// Returns the destination archetype.
    pub(crate) unsafe fn traverse_insert(
        &mut self,
        current_arch_idx: ArchetypeIdx,
        component_idx: ComponentIdx,
        components: &mut Components,
        systems: &mut Systems,
    ) -> ArchetypeIdx {
        let next_arch_idx = self.archetypes.vacant_key();

        let current_arch = unsafe {
            self.archetypes
                .get_debug_checked_mut(current_arch_idx.0 as usize)
        };

        match current_arch.insert_components.entry(component_idx) {
            Entry::Vacant(vacant_insert_components) => {
                let Err(idx) = current_arch
                    .columns
                    .binary_search_by_key(&component_idx, |c| c.component_idx)
                else {
                    // If this archetype already has the component, then stop here.
                    return current_arch_idx;
                };

                let mut new_components = Vec::with_capacity(current_arch.columns.len() + 1);
                new_components.extend(current_arch.columns.iter().map(|c| c.component_idx));
                new_components.insert(idx, component_idx);

                match self.by_components.entry(new_components.into_boxed_slice()) {
                    Entry::Vacant(vacant_by_components) => {
                        if next_arch_idx >= u32::MAX as usize {
                            panic!("too many archetypes");
                        }

                        let arch_id = ArchetypeIdx(next_arch_idx as u32);

                        let mut new_arch = Archetype::new(
                            arch_id,
                            vacant_by_components.key().iter().copied(),
                            components,
                        );

                        new_arch
                            .remove_components
                            .insert(component_idx, current_arch_idx);

                        /*
                        for &component_idx in vacant_by_components.key().iter() {
                            let info =
                                unsafe { components.by_index(component_idx).unwrap_debug_checked() };

                            assert!(info.member_of.insert(arch_id));
                        }

                        for ptr in systems.iter() {
                            let info = unsafe { &mut (*ptr.as_ptr()) };

                            let interested = unsafe {
                                info.system.refresh_archetype(
                                    RefreshArchetypeReason::New,
                                    arch_id,
                                    &new_arch,
                                )
                            };

                            if interested {
                                new_arch.subscribe(ptr);
                            }
                        }*/

                        vacant_by_components.insert(arch_id);

                        vacant_insert_components.insert(arch_id);

                        self.archetypes.insert(new_arch);

                        arch_id
                    }
                    Entry::Occupied(o) => *vacant_insert_components.insert(*o.get()),
                }
            }
            Entry::Occupied(o) => *o.get(),
        }
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

unsafe impl SparseIndex for ArchetypeIdx {
    const MAX: Self = ArchetypeIdx::NULL;

    fn index(self) -> usize {
        self.0.index()
    }

    fn from_index(idx: usize) -> Self {
        Self(u32::from_index(idx))
    }
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
    /// Systems that need to be notified about column changes.
    refresh_listeners: BTreeSet<SystemInfoPtr>,
    /// Entity event listeners for this archetype.
    event_listeners: SparseMap<EntityEventIdx, SystemList>,
}

impl Archetype {
    fn empty() -> Self {
        Self {
            index: ArchetypeIdx::EMPTY,
            entity_ids: vec![],
            columns: Box::new([]),
            insert_components: BTreeMap::new(),
            remove_components: BTreeMap::new(),
            refresh_listeners: BTreeSet::new(),
            event_listeners: SparseMap::new(),
        }
    }

    /// # Safety
    ///
    /// Iterator must be sorted in ascending order and all IDs must be valid.
    unsafe fn new(
        index: ArchetypeIdx,
        ids: impl IntoIterator<Item = ComponentIdx>,
        comps: &Components,
    ) -> Self {
        Self {
            entity_ids: vec![],
            columns: ids
                .into_iter()
                .map(|idx| {
                    let comp = unsafe {
                        comps
                            .by_index(idx)
                            .expect_debug_checked("invalid component ID")
                    };

                    Column {
                        data: unsafe { BlobVec::new(comp.layout(), comp.drop()) },
                        component_idx: idx,
                    }
                })
                .collect(),
            insert_components: BTreeMap::new(),
            remove_components: BTreeMap::new(),
            refresh_listeners: BTreeSet::new(),
            event_listeners: SparseMap::new(),
            index,
        }
    }

    /// Add an entity to this archetype.
    pub(crate) unsafe fn add_entity(
        &mut self,
        id: EntityId,
    ) -> (ArchetypeRow, impl Iterator<Item = NonNull<u8>> + '_) {
        debug_assert!(self.entity_ids.len() <= u32::MAX as usize);

        let row = ArchetypeRow(self.entity_ids.len() as u32);
        self.entity_ids.push(id);

        // TODO: refresh archetype notification for systems?

        let iter = self.columns.iter_mut().map(|col| col.data.push());

        (row, iter)
    }

    fn register_system(&mut self, info: &mut SystemInfo) {
        if info
            .component_access()
            .expr
            .eval(|idx| self.column_of(idx).is_some())
        {
            self.refresh_listeners.insert(info.ptr());
            unsafe {
                info.system_mut()
                    .refresh_archetype(RefreshArchetypeReason::New, self)
            };
        }

        if let (Some(expr), EventIdx::Entity(entity_event_idx)) =
            (info.entity_event_expr(), info.received_event().index())
        {
            if expr.eval(|idx| self.column_of(idx).is_some()) {
                if let Some(list) = self.event_listeners.get_mut(entity_event_idx) {
                    list.insert(info.ptr(), info.priority());
                } else {
                    let mut list = SystemList::new();
                    list.insert(info.ptr(), info.priority());

                    self.event_listeners.insert(entity_event_idx, list);
                }
            }
        }
    }

    pub(crate) fn system_list_for(&self, idx: EntityEventIdx) -> Option<&SystemList> {
        self.event_listeners.get(idx)
    }

    pub fn index(&self) -> ArchetypeIdx {
        self.index
    }

    pub fn entity_count(&self) -> u32 {
        self.entity_ids.len() as u32
    }

    pub fn columns(&self) -> &[Column] {
        &self.columns
    }

    pub fn column_of(&self, idx: ComponentIdx) -> Option<&Column> {
        let idx = self
            .columns
            .binary_search_by_key(&idx, |c| c.component_idx)
            .ok()?;

        Some(unsafe { self.columns.get_debug_checked(idx) })
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
