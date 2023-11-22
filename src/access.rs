use std::{fmt, mem};

use crate::bit_set::{BitSet, BitSetIndex};
use crate::component::ComponentId;

#[derive(Clone, Default, Debug)]
pub struct SystemAccess {
    pub received_event: Access,
    pub event_queue: Access,
    pub reserve_entity: Access,
    pub components: FilteredAccessExpr<ComponentId>,
}

impl SystemAccess {
    /// If the two systems described by `self` and `other` can safely run in
    /// parallel with each other (assuming they don't refer to the same system,
    /// since systems require `&mut self` to run.)
    pub fn is_compatible(&self, other: &Self) -> bool {
        self.received_event.is_compatible(other.received_event)
            && self.event_queue.is_compatible(other.event_queue)
            && self.reserve_entity.is_compatible(other.reserve_entity)
            && self.components.is_compatible(&other.components)
    }
}

pub struct FilteredAccessExpr<T> {
    /// Unfiltered accesses describing Reads and Writes.
    access: AccessMap<T>,
    /// The filters (With and Without) in disjunctive normal form. This is a
    /// series of ANDs joined by ORs.
    filters: Vec<AccessFilters<T>>,
}

impl<T: BitSetIndex + fmt::Debug> fmt::Debug for FilteredAccessExpr<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("FilteredAccessExpr")
            .field("access", &self.access)
            .field("filters", &self.filters)
            .finish()
    }
}

impl<T> FilteredAccessExpr<T> {
    pub fn new() -> Self {
        Self {
            access: AccessMap::new(),
            filters: vec![AccessFilters::default()],
        }
    }
}

impl<T> Clone for FilteredAccessExpr<T> {
    fn clone(&self) -> Self {
        Self {
            access: self.access.clone(),
            filters: self.filters.clone(),
        }
    }
}

impl<T> Default for FilteredAccessExpr<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T: BitSetIndex> FilteredAccessExpr<T> {
    pub fn and_read(&mut self, value: T) -> &mut Self {
        self.access.set(value, Access::Read);
        self.and_with(value)
    }

    pub fn and_read_write(&mut self, value: T) -> &mut Self {
        self.access.set(value, Access::ReadWrite);
        self.and_with(value)
    }

    pub fn and_with(&mut self, value: T) -> &mut Self {
        for f in &mut self.filters {
            f.with.insert(value);
        }
        self
    }

    pub fn and_without(&mut self, value: T) -> &mut Self {
        for f in &mut self.filters {
            f.without.insert(value);
        }
        self
    }

    pub fn and(&mut self, other: &Self) -> &mut Self {
        self.access.union(&other.access);

        let mut new_filters = Vec::with_capacity(self.filters.len() * other.filters.len());
        for filter in &self.filters {
            for other in &other.filters {
                let mut new_filter: AccessFilters<T> = filter.clone();

                new_filter.with.union(&other.with);
                new_filter.without.union(&other.without);

                new_filters.push(new_filter);
            }
        }

        self.filters = new_filters;
        self
    }

    pub fn or(&mut self, other: &Self) -> &mut Self {
        self.access.union(&other.access);

        self.filters.extend(other.filters.iter().cloned());

        self
    }

    pub fn not(&mut self) -> &mut Self {
        // Apply De Morgan's law to the filters and clear the access map.

        let mut res = Self::new();

        for mut filter in mem::take(&mut self.filters) {
            mem::swap(&mut filter.with, &mut filter.without);

            let expr = Self {
                access: AccessMap::new(),
                filters: vec![filter],
            };

            res.and(&expr);
        }

        *self = res;

        self
    }

    pub fn xor(&mut self, other: &Self) -> &mut Self {
        // A ⊻ B = (A ∧ ¬B) ∨ (B ∧ ¬A).

        let mut this = self.clone();
        let mut other = other.clone();

        self.and(other.clone().not()).or(other.and(this.not()))
    }

    #[must_use]
    pub fn is_read_compatible(&self, value: T) -> bool {
        self.access.get(value).is_compatible(Access::Read)
    }

    #[must_use]
    pub fn is_read_write_compatible(&self, value: T) -> bool {
        self.access.get(value).is_compatible(Access::ReadWrite)
    }

    /// Whether these two accesses can be active at the same time without
    /// conflicting.
    pub fn is_compatible(&self, other: &Self) -> bool {
        if self.access.is_compatible(&other.access) {
            // No need to check the filters if the accesses can't possibly conflict.
            return true;
        }

        // The unfiltered accesses are incompatible, so check if the filters would make
        // the unfiltered access compatible.
        //
        // Since the filters are in disjunctive normal form (ORs of ANDs), we need to
        // check that all pairs of filters from `self` and `other` are disjoint.
        self.filters.iter().all(|filter| {
            other
                .filters
                .iter()
                .all(|other_filter| filter.is_disjoint(other_filter))
        })
    }
}

/// A map from `T` to `Access`.
struct AccessMap<T> {
    read: BitSet<T>,
    write: BitSet<T>,
}

impl<T: BitSetIndex + fmt::Debug> fmt::Debug for AccessMap<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("AccessMap")
            .field("read", &self.read)
            .field("write", &self.write)
            .finish()
    }
}

impl<T> AccessMap<T> {
    pub(crate) fn new() -> Self {
        Self {
            read: BitSet::new(),
            write: BitSet::new(),
        }
    }

    pub(crate) fn clear(&mut self) {
        self.read.clear();
        self.write.clear();
    }
}

impl<T> Clone for AccessMap<T> {
    fn clone(&self) -> Self {
        Self {
            read: self.read.clone(),
            write: self.write.clone(),
        }
    }
}

impl<T: BitSetIndex> AccessMap<T> {
    fn get(&self, value: T) -> Access {
        match (self.read.contains(value), self.write.contains(value)) {
            (true, true) => Access::ReadWrite,
            (true, false) => Access::Read,
            (false, true) => unreachable!("read-write implies read"),
            (false, false) => Access::None,
        }
    }

    fn set(&mut self, value: T, access: Access) {
        match access {
            Access::None => {
                self.read.remove(value);
                self.write.remove(value);
            }
            Access::Read => {
                self.read.insert(value);
                self.write.remove(value);
            }
            Access::ReadWrite => {
                self.read.insert(value);
                self.write.insert(value);
            }
        }
    }

    fn union(&mut self, other: &Self) {
        self.read.union(&other.read);
        self.write.union(&other.write);
    }

    /// Whether these two accesses can be active at the same time without
    /// conflicting.
    #[must_use]
    fn is_compatible(&self, other: &Self) -> bool {
        self.read.is_disjoint(&other.write) && self.write.is_disjoint(&other.read)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default, Debug)]
pub(crate) enum Access {
    /// Cannot read or write to the data.
    #[default]
    None,
    /// Can read, but not write to the data (shared access).
    Read,
    /// Can both read and write to the data (exclusive access).
    ReadWrite,
}

impl Access {
    #[must_use]
    pub const fn is_compatible(self, other: Self) -> bool {
        match (self, other) {
            (Access::None, _)
            | (Access::Read, Access::None | Access::Read)
            | (Access::ReadWrite, Access::None) => true,
            _ => false,
        }
    }
}

/// Sets of `With` and `Without` filters.
/// Logically, this is set of variables ANDed together, possibly with negations.
/// Example: `A ∧ B ∧ ¬C ∧ ¬A`.
struct AccessFilters<T> {
    with: BitSet<T>,
    without: BitSet<T>,
}

impl<T: BitSetIndex> AccessFilters<T> {
    /// Determines if `self` and `other` are disjoint, i.e. if there is no
    /// combination of values the variables could have to make both expressions
    /// true at the same time.
    ///
    /// Examples:
    /// - `A` is not disjoint with `B`.
    /// - `A` is disjoint with `¬A`.
    /// - `A ∧ ¬A` is disjoint with `B ∧ C`.
    fn is_disjoint(&self, other: &Self) -> bool {
        !self.with.is_disjoint(&self.without)
            || !other.with.is_disjoint(&other.without)
            || !self.with.is_disjoint(&other.without)
            || !other.with.is_disjoint(&self.without)
    }
}

impl<T: BitSetIndex + fmt::Debug> fmt::Debug for AccessFilters<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("AccessFilters")
            .field("with", &self.with)
            .field("without", &self.without)
            .finish()
    }
}

impl<T> Clone for AccessFilters<T> {
    fn clone(&self) -> Self {
        Self {
            with: self.with.clone(),
            without: self.without.clone(),
        }
    }
}

impl<T> Default for AccessFilters<T> {
    fn default() -> Self {
        Self {
            with: Default::default(),
            without: Default::default(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn read_write_conflict_and_one() {
        let mut expr = FilteredAccessExpr::<usize>::new();

        assert!(expr.is_read_write_compatible(0));

        expr.and_read_write(0);
    }

    #[test]
    fn read_write_conflict_is_compatible() {
        let mut expr = FilteredAccessExpr::<usize>::new();
        expr.and_read_write(0);

        let expr2 = expr.clone();

        assert!(!expr.is_compatible(&expr2));
    }

    #[test]
    fn compatible_access_basic() {
        let mut expr = FilteredAccessExpr::<usize>::new();
        expr.and_read(0);
        expr.and_read_write(1);

        let mut expr2 = FilteredAccessExpr::<usize>::new();
        expr2.and_read(0);
        expr2.and_with(1);
        expr2.and_read_write(2);

        assert!(expr.is_compatible(&expr2));
    }

    #[test]
    fn disjoint_filters() {
        let mut expr = FilteredAccessExpr::<usize>::new();
        expr.and_read_write(0);
        expr.and_with(1);

        let mut expr2 = FilteredAccessExpr::<usize>::new();
        expr2.and_read_write(0);
        expr2.and_without(1);

        assert!(expr.is_compatible(&expr2));
    }

    #[test]
    fn filter_contradiction() {
        let mut expr = FilteredAccessExpr::<usize>::new();
        expr.and_read_write(0);
        expr.and_with(1);
        expr.and_without(1);

        let mut expr2 = FilteredAccessExpr::<usize>::new();
        expr2.and_read_write(0);

        assert!(expr.is_compatible(&expr2));
    }
}
