//! [`Archetype`] and related items.

use alloc::collections::btree_map::Entry as BTreeEntry;
use alloc::collections::{BTreeMap, BTreeSet};
#[cfg(not(feature = "std"))]
use alloc::{boxed::Box, vec, vec::Vec};
use core::cmp::Ordering;
use core::ops::Index;
use core::ptr::NonNull;
use core::{fmt, mem, ptr, slice};

use ahash::RandomState;
use slab::Slab;

use crate::aliased_box::AliasedBox;
use crate::assert::{assume_debug_checked, GetDebugChecked, UnwrapDebugChecked};
use crate::blob_vec::BlobVec;
use crate::component::{ComponentIdx, ComponentInfo, Components};
use crate::entity::{Entities, EntityId, EntityLocation};
use crate::event::{EventIdx, EventPtr, TargetedEventIdx};
use crate::handler::{
    HandlerConfig, HandlerInfo, HandlerInfoPtr, HandlerList, HandlerParam, Handlers, InitError,
};
use crate::map::{Entry, HashMap};
use crate::prelude::World;
use crate::sparse::SparseIndex;
use crate::sparse_map::SparseMap;
use crate::world::UnsafeWorldCell;

/// Contains all the [`Archetype`]s and their metadata for a world.
///
/// This can be obtained in a handler by using the `&Archetypes` handler
/// parameter.
///
/// ```
/// # use evenio::prelude::*;
/// # use evenio::archetype::Archetypes;
/// #
/// # #[derive(Event)] struct E;
/// #
/// # let mut world = World::new();
/// world.add_handler(|_: Receiver<E>, archetypes: &Archetypes| {});
/// ```
#[derive(Debug)]
pub struct Archetypes {
    archetypes: Slab<Archetype>,
    by_components: HashMap<AliasedBox<[ComponentIdx]>, ArchetypeIdx>,
}

impl Archetypes {
    pub(crate) fn new() -> Self {
        let mut map = HashMap::with_hasher(RandomState::new());
        map.insert(vec![].into_boxed_slice().into(), ArchetypeIdx::EMPTY);

        Self {
            archetypes: Slab::from_iter([(0, Archetype::empty())]),
            by_components: map,
        }
    }

    /// Returns a reference to the empty archetype (The archetype with no
    /// components).
    ///
    /// The empty archetype is always present, so this function is infallible.
    pub fn empty(&self) -> &Archetype {
        // SAFETY: The empty archetype is always at index 0.
        unsafe { self.archetypes.get_debug_checked(0) }
    }

    /// Returns a mutable reference to the empty archetype.
    pub(crate) fn empty_mut(&mut self) -> &mut Archetype {
        // SAFETY: The empty archetype is always at index 0.
        unsafe { self.archetypes.get_debug_checked_mut(0) }
    }

    /// Gets a reference to the archetype identified by the given
    /// [`ArchetypeIdx`]. Returns `None` if the index is invalid.
    pub fn get(&self, idx: ArchetypeIdx) -> Option<&Archetype> {
        self.archetypes.get(idx.0 as usize)
    }

    /// Gets a reference to the archetype with the given set of components.
    ///
    /// Returns `None` if there is no archetype with the given set of
    /// components or the given [`ComponentIdx`] slice is not sorted and
    /// deduplicated.
    pub fn get_by_components(&self, components: &[ComponentIdx]) -> Option<&Archetype> {
        let idx = *self.by_components.get(components)?;
        Some(unsafe { self.get(idx).unwrap_debug_checked() })
    }

    /// Spawns a new entity into the empty archetype with the given ID and
    /// returns its location.
    pub(crate) fn spawn(&mut self, id: EntityId) -> EntityLocation {
        let empty = self.empty_mut();

        let rellocated = empty.push_would_reallocate();

        let row = ArchetypeRow(empty.entity_count());
        empty.entity_ids.push(id);

        if empty.entity_count() == 1 || rellocated {
            for mut ptr in empty.refresh_listeners.iter().copied() {
                unsafe { ptr.as_info_mut().handler_mut().refresh_archetype(empty) };
            }
        }

        EntityLocation {
            archetype: ArchetypeIdx::EMPTY,
            row,
        }
    }

    /// Returns an iterator over all archetypes in an arbitrary order.
    pub fn iter(&self) -> impl Iterator<Item = &Archetype> {
        self.archetypes.iter().map(|(_, v)| v)
    }

    /// Returns a count of the archetypes.
    pub fn len(&self) -> usize {
        self.archetypes.len()
    }

    pub(crate) fn register_handler(&mut self, info: &mut HandlerInfo) {
        // TODO: use a `Component -> Vec<Archetype>` index to make this faster?
        for (_, arch) in &mut self.archetypes {
            arch.register_handler(info);
        }
    }

    pub(crate) fn remove_handler(&mut self, info: &HandlerInfo) {
        // TODO: use a `Component -> Vec<Archetype>` index to make this faster?
        for (_, arch) in &mut self.archetypes {
            arch.refresh_listeners.remove(&info.ptr());

            if let EventIdx::Targeted(idx) = info.received_event().index() {
                if let Some(list) = arch.event_listeners.get_mut(idx) {
                    list.remove(info.ptr());
                }
            }
        }
    }

    pub(crate) fn remove_component<F>(
        &mut self,
        info: &mut ComponentInfo,
        components: &mut Components,
        mut f: F,
    ) where
        F: FnMut(EntityId),
    {
        let removed_component_id = info.id();

        for arch_idx in info.member_of.drain(..) {
            let mut arch = self.archetypes.remove(arch_idx.0 as usize);

            for mut ptr in arch.refresh_listeners.iter().copied() {
                unsafe { ptr.as_info_mut().handler_mut().remove_archetype(&arch) };
            }

            for &comp_idx in arch.component_indices() {
                if comp_idx != removed_component_id.index() {
                    let info =
                        unsafe { components.get_by_index_mut(comp_idx).unwrap_debug_checked() };
                    info.member_of.swap_remove(&arch_idx);
                }
            }

            // NOTE: Using plain `.remove()` here makes Miri sad.
            self.by_components.remove_entry(arch.component_indices());

            for &entity_id in arch.entity_ids() {
                f(entity_id);
            }

            // Remove all references to the removed archetype.

            for (comp_idx, arch_idx) in mem::take(&mut arch.insert_components) {
                let other_arch =
                    unsafe { self.archetypes.get_debug_checked_mut(arch_idx.0 as usize) };

                other_arch.remove_components.remove(&comp_idx);
            }

            for (comp_idx, arch_idx) in mem::take(&mut arch.remove_components) {
                let other_arch =
                    unsafe { self.archetypes.get_debug_checked_mut(arch_idx.0 as usize) };

                other_arch.insert_components.remove(&comp_idx);
            }
        }
    }

    /// Traverses one edge of the archetype graph in the insertion direction.
    /// Returns the destination archetype.
    ///
    /// If the archetype with the added component does not exist, then it is
    /// created. Otherwise, the existing archetype is returned.
    pub(crate) unsafe fn traverse_insert(
        &mut self,
        src_arch_idx: ArchetypeIdx,
        component_idx: ComponentIdx,
        components: &mut Components,
        handlers: &mut Handlers,
    ) -> ArchetypeIdx {
        debug_assert!(components.get_by_index(component_idx).is_some());

        let next_arch_idx = self.archetypes.vacant_key();

        let src_arch = unsafe {
            self.archetypes
                .get_debug_checked_mut(src_arch_idx.0 as usize)
        };

        match src_arch.insert_components.entry(component_idx) {
            BTreeEntry::Vacant(vacant_insert_components) => {
                let Err(idx) = src_arch
                    .component_indices
                    .as_ref()
                    .binary_search(&component_idx)
                else {
                    // Archetype already has this component.
                    return src_arch_idx;
                };

                let mut new_components = Vec::with_capacity(src_arch.component_indices.len() + 1);
                new_components.extend(src_arch.component_indices.as_ref().iter().copied());
                new_components.insert(idx, component_idx);

                match self
                    .by_components
                    .entry(new_components.into_boxed_slice().into())
                {
                    Entry::Vacant(vacant_by_components) => {
                        assert!(next_arch_idx < u32::MAX as usize, "too many archetypes");

                        let arch_id = ArchetypeIdx(next_arch_idx as u32);

                        let mut new_arch = Archetype::new(
                            arch_id,
                            vacant_by_components.key().as_ref().into(),
                            components,
                        );

                        new_arch
                            .remove_components
                            .insert(component_idx, src_arch_idx);

                        for info in handlers.iter_mut() {
                            new_arch.register_handler(info);
                        }

                        vacant_by_components.insert(arch_id);

                        vacant_insert_components.insert(arch_id);

                        self.archetypes.insert(new_arch);

                        arch_id
                    }
                    Entry::Occupied(o) => *vacant_insert_components.insert(*o.get()),
                }
            }
            BTreeEntry::Occupied(o) => *o.get(),
        }
    }

    /// Traverses one edge of the archetype graph in the remove direction.
    /// Returns the destination archetype.
    ///
    /// If the archetype with the removed component does not exist, then it is
    /// created. Otherwise, the existing archetype is returned.
    pub(crate) unsafe fn traverse_remove(
        &mut self,
        src_arch_idx: ArchetypeIdx,
        component_idx: ComponentIdx,
        components: &mut Components,
        handlers: &mut Handlers,
    ) -> ArchetypeIdx {
        let next_arch_idx = self.archetypes.vacant_key();

        let src_arch = unsafe {
            self.archetypes
                .get_debug_checked_mut(src_arch_idx.0 as usize)
        };

        match src_arch.remove_components.entry(component_idx) {
            BTreeEntry::Vacant(vacant_remove_components) => {
                if src_arch
                    .component_indices
                    .as_ref()
                    .binary_search(&component_idx)
                    .is_err()
                {
                    // Archetype already doesn't have the component.
                    return src_arch_idx;
                }

                let new_components: Box<[ComponentIdx]> = src_arch
                    .component_indices
                    .as_ref()
                    .iter()
                    .copied()
                    .filter(|&c| c != component_idx)
                    .collect();

                match self.by_components.entry(new_components.into()) {
                    Entry::Vacant(vacant_by_components) => {
                        assert!(next_arch_idx < u32::MAX as usize, "too many archetypes");

                        let arch_id = ArchetypeIdx(next_arch_idx as u32);

                        let mut new_arch = Archetype::new(
                            arch_id,
                            vacant_by_components.key().as_ref().into(),
                            components,
                        );

                        new_arch
                            .insert_components
                            .insert(component_idx, src_arch_idx);

                        for info in handlers.iter_mut() {
                            new_arch.register_handler(info);
                        }

                        vacant_by_components.insert(arch_id);

                        vacant_remove_components.insert(arch_id);

                        self.archetypes.insert(new_arch);

                        arch_id
                    }
                    Entry::Occupied(o) => *vacant_remove_components.insert(*o.get()),
                }
            }
            BTreeEntry::Occupied(o) => *o.get(),
        }
    }

    /// Move an entity from one archetype to another. Returns the entity's row
    /// in the new archetype.
    pub(crate) unsafe fn move_entity(
        &mut self,
        src: EntityLocation,
        dst: ArchetypeIdx,
        new_components: impl IntoIterator<Item = (ComponentIdx, *const u8)>,
        entities: &mut Entities,
    ) -> ArchetypeRow {
        let mut new_components = new_components.into_iter();

        if src.archetype == dst {
            let arch = self
                .archetypes
                .get_mut(src.archetype.0 as usize)
                .unwrap_debug_checked();

            for (comp_idx, comp_ptr) in new_components {
                let col = arch.column_of_mut(comp_idx).unwrap_debug_checked();

                col.data.assign(src.row.0 as usize, comp_ptr);
            }

            return src.row;
        }

        let (src_arch, dst_arch) = self
            .archetypes
            .get2_mut(src.archetype.0 as usize, dst.0 as usize)
            .unwrap();

        let dst_row = ArchetypeRow(dst_arch.entity_ids.len() as u32);

        let dst_arch_reallocated = dst_arch.push_would_reallocate();

        let mut src_idx = 0;
        let mut dst_idx = 0;

        loop {
            let src_in_bounds = src_idx < src_arch.component_indices.len();
            let dst_in_bounds = dst_idx < dst_arch.component_indices.len();

            match (src_in_bounds, dst_in_bounds) {
                (true, true) => {
                    let src_comp_idx = *src_arch
                        .component_indices
                        .as_ref()
                        .get_debug_checked(src_idx);

                    let dst_comp_idx = *dst_arch
                        .component_indices
                        .as_ref()
                        .get_debug_checked(dst_idx);

                    match src_comp_idx.cmp(&dst_comp_idx) {
                        Ordering::Less => {
                            let src_col = &mut *src_arch.columns.as_ptr().add(src_idx);
                            src_col.data.swap_remove(src.row.0 as usize);
                            src_idx += 1;
                        }
                        Ordering::Equal => {
                            let src_col = &mut *src_arch.columns.as_ptr().add(src_idx);
                            let dst_col = &mut *dst_arch.columns.as_ptr().add(dst_idx);

                            src_col
                                .data
                                .transfer_elem(&mut dst_col.data, src.row.0 as usize);

                            src_idx += 1;
                            dst_idx += 1;
                        }
                        Ordering::Greater => {
                            let (component_idx, component_ptr) =
                                new_components.next().unwrap_debug_checked();

                            let dst_col = &mut *dst_arch.columns.as_ptr().add(dst_idx);

                            let dst_comp_idx = *dst_arch
                                .component_indices
                                .as_ref()
                                .get_debug_checked(dst_idx);

                            debug_assert_eq!(component_idx, dst_comp_idx);

                            ptr::copy_nonoverlapping(
                                component_ptr,
                                dst_col.data.push().as_ptr(),
                                dst_col.data.elem_layout().size(),
                            );

                            dst_idx += 1;
                        }
                    }
                }
                (true, false) => {
                    let src_col = &mut *src_arch.columns.as_ptr().add(src_idx);
                    src_col.data.swap_remove(src.row.0 as usize);
                    src_idx += 1;
                }
                (false, true) => {
                    let (component_idx, component_ptr) =
                        new_components.next().unwrap_debug_checked();

                    let dst_col = &mut *dst_arch.columns.as_ptr().add(dst_idx);

                    let dst_comp_idx = *dst_arch
                        .component_indices
                        .as_ref()
                        .get_debug_checked(dst_idx);

                    debug_assert_eq!(component_idx, dst_comp_idx);

                    ptr::copy_nonoverlapping(
                        component_ptr,
                        dst_col.data.push().as_ptr(),
                        dst_col.data.elem_layout().size(),
                    );

                    dst_idx += 1;
                }
                (false, false) => break,
            }
        }

        debug_assert!(new_components.next().is_none());

        let entity_id = src_arch.entity_ids.swap_remove(src.row.0 as usize);
        dst_arch.entity_ids.push(entity_id);

        *unsafe { entities.get_mut(entity_id).unwrap_debug_checked() } = EntityLocation {
            archetype: dst,
            row: dst_row,
        };

        if let Some(&swapped_entity_id) = src_arch.entity_ids.get(src.row.0 as usize) {
            unsafe { entities.get_mut(swapped_entity_id).unwrap_debug_checked() }.row = src.row;
        }

        if src_arch.entity_ids.is_empty() {
            for mut ptr in src_arch.refresh_listeners.iter().copied() {
                unsafe { ptr.as_info_mut().handler_mut().remove_archetype(src_arch) };
            }
        }

        if dst_arch_reallocated || dst_arch.entity_count() == 1 {
            for mut ptr in dst_arch.refresh_listeners.iter().copied() {
                unsafe { ptr.as_info_mut().handler_mut().refresh_archetype(dst_arch) };
            }
        }

        dst_row
    }

    pub(crate) fn remove_entity(&mut self, entity: EntityId, entities: &mut Entities) {
        let Some(loc) = entities.remove(entity) else {
            return;
        };

        let arch = unsafe {
            self.archetypes
                .get_debug_checked_mut(loc.archetype.0 as usize)
        };

        for col in arch.columns_mut() {
            unsafe { col.data.swap_remove(loc.row.0 as usize) };
        }

        unsafe {
            assume_debug_checked((loc.row.0 as usize) < arch.entity_ids.len());
        };

        arch.entity_ids.swap_remove(loc.row.0 as usize);

        if (loc.row.0 as usize) < arch.entity_ids.len() {
            let displaced = *unsafe { arch.entity_ids.get_debug_checked(loc.row.0 as usize) };
            unsafe { entities.get_mut(displaced).unwrap_debug_checked() }.row = loc.row;
        }

        if arch.entity_count() == 0 {
            for mut ptr in arch.refresh_listeners.iter().copied() {
                unsafe { ptr.as_info_mut().handler_mut().remove_archetype(arch) };
            }
        }
    }
}

impl Index<ArchetypeIdx> for Archetypes {
    type Output = Archetype;

    /// Panics if the index is invalid.
    fn index(&self, index: ArchetypeIdx) -> &Self::Output {
        if let Some(arch) = self.get(index) {
            arch
        } else {
            panic!("no such archetype with index of {index:?} exists")
        }
    }
}

unsafe impl HandlerParam for &'_ Archetypes {
    type State = ();

    type This<'a> = &'a Archetypes;

    fn init(_world: &mut World, _config: &mut HandlerConfig) -> Result<Self::State, InitError> {
        Ok(())
    }

    unsafe fn get<'a>(
        _state: &'a mut Self::State,
        _info: &'a HandlerInfo,
        _event_ptr: EventPtr<'a>,
        _target_location: EntityLocation,
        world: UnsafeWorldCell<'a>,
    ) -> Self::This<'a> {
        world.archetypes()
    }

    fn refresh_archetype(_state: &mut Self::State, _arch: &Archetype) {}

    fn remove_archetype(_state: &mut Self::State, _arch: &Archetype) {}
}

/// Unique identifier for an archetype.
///
/// Old archetype indices may be reused by new archetypes.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
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

/// Offset from the beginning of a component column. Combined with an
/// [`ArchetypeIdx`], this can identify the location of an entity.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct ArchetypeRow(pub u32);

impl ArchetypeRow {
    /// The archetype row that is always invalid.
    pub const NULL: Self = Self(u32::MAX);
}

/// A container for all entities with a particular set of components.
///
/// Each component has a corresponding columnm of contiguous data in
/// this archetype. Each row in is then a separate entity.
///
/// For instance, an archetype with component set `{A, B, C}` might look like:
///
/// | Entity ID | Column A | Column B | Column C |
/// |-----------|----------|----------|----------|
/// | Entity 0  | "foo"    | 123      | true     |
/// | Entity 1  | "bar"    | 456      | false    |
/// | Entity 2  | "baz"    | 789      | true     |
pub struct Archetype {
    /// The index of this archetype. Provided here for convenience.
    index: ArchetypeIdx,
    /// Component indices of this archetype, one per column in sorted order.
    component_indices: NonNull<[ComponentIdx]>,
    /// Columns of component data in this archetype. Sorted by component index.
    ///
    /// This is a `Box<[Column]>` with the length stripped out. The length field
    /// would be redundant since it's always the same as `component_indices`.
    columns: NonNull<Column>,
    /// A special column containing the [`EntityId`] for all entities in the
    /// archetype.
    entity_ids: Vec<EntityId>,
    insert_components: BTreeMap<ComponentIdx, ArchetypeIdx>,
    remove_components: BTreeMap<ComponentIdx, ArchetypeIdx>,
    /// Handlers that need to be notified about column changes.
    refresh_listeners: BTreeSet<HandlerInfoPtr>,
    /// Targeted event listeners for this archetype.
    event_listeners: SparseMap<TargetedEventIdx, HandlerList>,
}

impl Archetype {
    fn empty() -> Self {
        Self {
            index: ArchetypeIdx::EMPTY,
            component_indices: NonNull::from(<&[_]>::default()),
            columns: NonNull::dangling(),
            entity_ids: vec![],
            insert_components: BTreeMap::new(),
            remove_components: BTreeMap::new(),
            refresh_listeners: BTreeSet::new(),
            event_listeners: SparseMap::new(),
        }
    }

    /// # Safety
    ///
    /// - Component indices slice must be in sorted order.
    /// - Component indices slice must outlive the archetype.
    /// - All component indices must be valid.
    unsafe fn new(
        arch_idx: ArchetypeIdx,
        component_indices: NonNull<[ComponentIdx]>,
        components: &mut Components,
    ) -> Self {
        let columns: Box<[Column]> = component_indices
            .as_ref()
            .iter()
            .map(|&idx| {
                let info = unsafe {
                    components
                        .get_by_index_mut(idx)
                        .expect_debug_checked("invalid component index")
                };

                info.member_of.insert(arch_idx);

                Column {
                    data: unsafe { BlobVec::new(info.layout(), info.drop()) },
                }
            })
            .collect();

        // SAFETY: `Box::into_raw` guarantees non-null.
        let columns_ptr = unsafe { NonNull::new_unchecked(Box::into_raw(columns) as *mut Column) };

        Self {
            index: arch_idx,
            component_indices,
            columns: columns_ptr,
            entity_ids: vec![],
            insert_components: BTreeMap::new(),
            remove_components: BTreeMap::new(),
            refresh_listeners: BTreeSet::new(),
            event_listeners: SparseMap::new(),
        }
    }

    fn register_handler(&mut self, info: &mut HandlerInfo) {
        if info
            .archetype_filter()
            .matches_archetype(|idx| self.column_of(idx).is_some())
        {
            if self.entity_count() > 0 {
                info.handler_mut().refresh_archetype(self);
            }

            self.refresh_listeners.insert(info.ptr());
        }

        if let (Some(expr), EventIdx::Targeted(targeted_event_idx)) = (
            info.targeted_event_component_access(),
            info.received_event().index(),
        ) {
            if expr.matches_archetype(|idx| self.column_of(idx).is_some()) {
                if let Some(list) = self.event_listeners.get_mut(targeted_event_idx) {
                    list.insert(info.ptr(), info.priority());
                } else {
                    let mut list = HandlerList::new();
                    list.insert(info.ptr(), info.priority());

                    self.event_listeners.insert(targeted_event_idx, list);
                }
            }
        }
    }

    pub(crate) fn handler_list_for(&self, idx: TargetedEventIdx) -> Option<&HandlerList> {
        self.event_listeners.get(idx)
    }

    /// Returns the index of this archetype.
    pub fn index(&self) -> ArchetypeIdx {
        self.index
    }

    /// Returns the total number of entities in this archetype.
    pub fn entity_count(&self) -> u32 {
        debug_assert!(u32::try_from(self.entity_ids.len()).is_ok());
        // This doesn't truncate because entity indices are less than u32::MAX.
        self.entity_ids.len() as u32
    }

    /// Returns a slice of [`EntityId`]s for all the entities in this archetype.
    pub fn entity_ids(&self) -> &[EntityId] {
        &self.entity_ids
    }

    /// Returns a sorted slice of component types for every column in this
    /// archetype.
    ///
    /// The returned slice has the same length as the slice returned by
    /// [`columns`](Archetype::columns).
    pub fn component_indices(&self) -> &[ComponentIdx] {
        unsafe { self.component_indices.as_ref() }
    }

    /// Returns a slice of columns sorted by [`ComponentIdx`].
    pub fn columns(&self) -> &[Column] {
        unsafe { slice::from_raw_parts(self.columns.as_ptr(), self.component_indices.len()) }
    }

    /// Returns a slice of columns sorted by [`ComponentIdx`].
    fn columns_mut(&mut self) -> &mut [Column] {
        unsafe { slice::from_raw_parts_mut(self.columns.as_ptr(), self.component_indices.len()) }
    }

    /// Finds the column with the given component. Returns `None` if it doesn't
    /// exist.
    pub fn column_of(&self, idx: ComponentIdx) -> Option<&Column> {
        let idx = self.component_indices().binary_search(&idx).ok()?;

        // SAFETY: `binary_search` ensures `idx` is in bounds.
        Some(unsafe { &*self.columns.as_ptr().add(idx) })
    }

    fn column_of_mut(&mut self, idx: ComponentIdx) -> Option<&mut Column> {
        let idx = self.component_indices().binary_search(&idx).ok()?;

        // SAFETY: `binary_search` ensures `idx` is in bounds.
        Some(unsafe { &mut *self.columns.as_ptr().add(idx) })
    }

    /// Would the columns of this archetype reallocate if an entity were added
    /// to it?
    fn push_would_reallocate(&self) -> bool {
        // All columns should have the same capacity and length, so we only need to look
        // at one of them. The `Vec` holding the Entity IDs might have a different
        // reallocation strategy, so check that too.
        self.columns()
            .first()
            .map_or(false, |col| col.data.len() == col.data.capacity())
            || self.entity_ids.capacity() == self.entity_ids.len()
    }
}

impl Drop for Archetype {
    fn drop(&mut self) {
        // Free the array of columns by constructing a `Box<[Column]>` and immediately
        // dropping it.
        //
        // SAFETY: The columns pointer originated from a
        // `Box<[Column]>` with the length of `component_indices`.
        let _ = unsafe {
            Box::from_raw(slice::from_raw_parts_mut(
                self.columns.as_ptr(),
                self.component_indices.len(),
            ))
        };
    }
}

impl fmt::Debug for Archetype {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Archetype")
            .field("index", &self.index)
            .field("component_indices", &self.component_indices())
            .field("columns", &self.columns())
            .field("entity_ids", &self.entity_ids)
            .field("insert_components", &self.insert_components)
            .field("remove_components", &self.remove_components)
            .field("refresh_listeners", &self.refresh_listeners)
            .field("event_listeners", &self.event_listeners)
            .finish()
    }
}

/// All of the component data for a single component type in an [`Archetype`].
#[derive(Debug)]
pub struct Column {
    /// Component data in this column.
    data: BlobVec,
}

impl Column {
    /// Returns a pointer to the beginning of the buffer holding the component
    /// data, or a dangling pointer if the the buffer is empty.
    pub fn data(&self) -> NonNull<u8> {
        self.data.as_ptr()
    }
}

#[cfg(test)]
mod tests {
    use crate::prelude::*;

    #[derive(Component)]
    struct C(String);

    #[derive(Event)]
    struct E1;

    #[derive(Event)]
    struct E2;

    #[test]
    fn insert_overwrites() {
        let mut world = World::new();

        let e = world.spawn();

        world.insert(e, C("hello".into()));

        assert_eq!(world.get::<C>(e).unwrap().0, "hello");

        world.insert(e, C("goodbye".into()));

        assert_eq!(world.get::<C>(e).unwrap().0, "goodbye");
    }
}
