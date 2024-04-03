use core::fmt;
use core::iter::FusedIterator;
use core::ptr::NonNull;

use crate::archetype::{ArchetypeIdx, ArchetypeRow, Archetypes};
use crate::assert::assume_debug_checked;
use crate::query::{Query, ReadOnlyQuery};

/// Iterator over entities matching the query `Q`.
///
/// Entities are visited in a deterministic but otherwise unspecified order.
#[must_use = "iterators are lazy and do nothing unless consumed"]
pub struct Iter<'a, Q: Query> {
    /// Pointer into the array of archetype states. This pointer moves forward
    /// until it reaches `state_last`.
    state: NonNull<Q::ArchState>,
    /// Pointer to the last arch state, or dangling if there are no arch states.
    /// This is _not_ a one-past-the-end pointer.
    state_last: NonNull<Q::ArchState>,
    /// Pointer into the array of archetype indices. This pointer moves forward
    /// in lockstep with `state`.
    index: NonNull<ArchetypeIdx>,
    /// Current row of the current archetype.
    row: ArchetypeRow,
    /// Number of entities in the current archetype.
    len: u32,
    archetypes: &'a Archetypes,
}

impl<'a, Q: Query> Iter<'a, Q> {
    #[inline]
    pub(super) unsafe fn new(
        indices: [ArchetypeIdx],
        states: [Q::ArchState],
        archetypes: &Archetypes,
    ) -> Self {
        assume_debug_checked(indices.len() == states.len());

        if states.is_empty() {
            Iter {
                state: NonNull::dangling(),
                state_last: NonNull::dangling(),
                index: NonNull::dangling(),
                row: ArchetypeRow(0),
                len: 0,
                archetypes,
            }
        } else {
            let start = states.as_ptr().cast_mut();
            let end = start.add(states.len() - 1);
            Iter {
                state: NonNull::new(start).unwrap_debug_checked(),
                state_last: NonNull::new(end).unwrap_debug_checked(),
                index: NonNull::new(indices.as_ptr().cast_mut()).unwrap_debug_checked(),
                row: ArchetypeRow(0),
                len: archetypes
                    .get(indices[0])
                    .unwrap_debug_checked()
                    .entity_count(),
                archetypes,
            }
        }
    }
}

impl<'a, Q: Query> Iterator for Iter<'a, Q> {
    type Item = Q::Item<'a>;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        if self.row.0 == self.len {
            if self.state == self.state_last {
                return None;
            }

            self.state = unsafe { NonNull::new_unchecked(self.state.as_ptr().add(1)) };
            self.index = unsafe { NonNull::new_unchecked(self.index.as_ptr().add(1)) };

            let idx = unsafe { *self.index.as_ptr() };
            let arch = unsafe { self.archetypes.get(idx).unwrap_debug_checked() };

            self.row = ArchetypeRow(0);
            self.len = arch.entity_count();

            // SAFETY: Fetcher state only contains nonempty archetypes.
            unsafe { assume_debug_checked(self.len > 0) };
        }

        let state = unsafe { &*self.state.as_ptr().cast_const() };
        let item = unsafe { Q::get(state, self.row) };

        self.row.0 += 1;

        Some(item)
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let len = self.len();
        (len, Some(len))
    }
}

impl<Q: Query> ExactSizeIterator for Iter<'_, Q> {
    fn len(&self) -> usize {
        let mut remaining = self.len - self.row.0;

        let mut index = self.index.as_ptr();

        let index_last = unsafe {
            // TODO: use `.sub_ptr` when stabilized.
            index.add(self.state_last.as_ptr().offset_from(self.state.as_ptr()) as usize)
        };

        while index != index_last {
            index = unsafe { index.add(1) };

            remaining +=
                unsafe { self.archetypes.get(*index).unwrap_debug_checked() }.entity_count();
        }

        remaining as usize
    }
}

impl<Q: Query> FusedIterator for Iter<'_, Q> {}

// SAFETY: Iter is only cloneable when the query is read-only.
impl<'a, Q: ReadOnlyQuery> Clone for Iter<'a, Q> {
    fn clone(&self) -> Self {
        Self {
            state: self.state,
            state_last: self.state_last,
            index: self.index,
            row: self.row,
            len: self.len,
            archetypes: self.archetypes,
        }
    }
}

impl<Q: Query> fmt::Debug for Iter<'_, Q> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Iter")
            .field("state", &self.state)
            .field("state_last", &self.state_last)
            .field("index", &self.index)
            .field("row", &self.row)
            .field("len", &self.len)
            .field("archetypes", &self.archetypes)
            .finish()
    }
}

// SAFETY: `Iter` iterates over component data only, which is always `Send` and
// `Sync`.
unsafe impl<Q: Query> Send for Iter<'_, Q> {}
unsafe impl<Q: Query> Sync for Iter<'_, Q> {}
