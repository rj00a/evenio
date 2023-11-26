use std::{fmt, mem};

use crate::bit_set::{BitSet, BitSetIndex};
use crate::component::ComponentId;

#[derive(Clone, Debug)]
pub struct SystemAccess {
    pub received_event: Access,
    pub event_queue: Access,
    pub reserve_entity: Access,
    pub components: AccessExpr<ComponentId>,
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

impl Default for SystemAccess {
    fn default() -> Self {
        Self {
            received_event: Default::default(),
            event_queue: Default::default(),
            reserve_entity: Default::default(),
            components: AccessExpr::one(),
        }
    }
}

pub struct AccessExpr<T> {
    /// Describes the potential Reads and Writes of this expression. This is
    /// used to prevent queries like `(&mut A, &A)` from being considered valid.
    access: AccessMap<T>,
    /// A boolean expression in disjunctive normal form,
    /// e.g. (A ∧ B ∧ ¬C) ∨ (D ∧ ¬E ∧ ¬F).
    disjunctions: Vec<Conjunctions<T>>,
}

impl<T: BitSetIndex + fmt::Debug> fmt::Debug for AccessExpr<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        struct DebugDisjunctions<'a, T>(&'a [Conjunctions<T>]);

        impl<T: fmt::Debug + BitSetIndex> fmt::Debug for DebugDisjunctions<'_, T> {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                if self.0.is_empty() {
                    write!(f, "⊥")?;
                } else {
                    let mut first = true;

                    for conj in self.0.iter() {
                        if !first {
                            write!(f, " ∨ ")?;
                        }
                        first = false;

                        if conj.with.is_empty() && conj.without.is_empty() {
                            write!(f, "⊤")?;
                        } else {
                            let mut first = true;

                            for val in conj.with.iter() {
                                if !first {
                                    write!(f, " ∧ ")?;
                                }
                                first = false;

                                write!(f, "{val:?}")?;
                            }

                            for val in conj.without.iter() {
                                if !first {
                                    write!(f, " ∧ ")?;
                                }
                                first = false;

                                write!(f, "¬{val:?}")?;
                            }
                        }
                    }
                }

                Ok(())
            }
        }

        f.debug_struct("FilteredAccessExpr")
            .field("access", &self.access)
            .field("filters", &DebugDisjunctions(&self.disjunctions))
            .finish()
    }
}

impl<T> AccessExpr<T> {
    /// Returns a new access expression representing `true` or `1`. This is the
    /// identity element for `∧`.
    pub fn one() -> Self {
        Self {
            access: AccessMap::new(),
            disjunctions: vec![Conjunctions::default()],
        }
    }

    /// Returns a new access expression representing `false` or `0`. This is the
    /// identity element for `∨`.
    pub fn zero() -> Self {
        Self {
            access: AccessMap::new(),
            disjunctions: vec![],
        }
    }
}

impl<T> Clone for AccessExpr<T> {
    fn clone(&self) -> Self {
        Self {
            access: self.access.clone(),
            disjunctions: self.disjunctions.clone(),
        }
    }
}

impl<T: BitSetIndex> AccessExpr<T> {
    pub fn with(value: T, access: Access) -> Self {
        let mut res = Self::zero();

        res.access.set(value, access);

        let mut conj = Conjunctions::default();
        conj.with.insert(value);
        res.disjunctions.push(conj);

        res
    }

    pub fn without(value: T) -> Self {
        let mut res = Self::zero();

        let mut conj = Conjunctions::default();
        conj.without.insert(value);
        res.disjunctions.push(conj);

        res
    }

    /// Performs an AND (`∧`) with `self` and `other` and stores the result in
    /// `self`. `self` is then returned for further chaining.
    pub fn and(&mut self, other: &Self) -> &mut Self {
        self.access.union(&other.access);

        let mut new_filters = Vec::new();
        for filter in &self.disjunctions {
            for other in &other.disjunctions {
                let mut new_filter: Conjunctions<T> = filter.clone();

                new_filter.with.union(&other.with);
                new_filter.without.union(&other.without);

                // Skip contradictions.
                if new_filter.with.is_disjoint(&new_filter.without) {
                    new_filters.push(new_filter);
                }
            }
        }

        self.disjunctions = new_filters;
        self
    }

    /// Performs an OR (`∨`) with `self` and `other` and stores the result in
    /// `self`. `self` is then returned for further chaining.
    pub fn or(&mut self, other: &Self) -> &mut Self {
        self.access.union(&other.access);

        self.disjunctions.extend(other.disjunctions.iter().cloned());

        self
    }

    /// Performs a unary NOT (`¬`) on `self` and stores the result in `self`.
    /// `self` is then returned for further chaining.
    pub fn not(&mut self) -> &mut Self {
        // Apply De Morgan's laws and clear the access map.

        let mut res = Self::one();

        for mut conj in mem::take(&mut self.disjunctions) {
            let mut ors = Self::zero();

            mem::swap(&mut conj.with, &mut conj.without);

            for with in conj.with.iter() {
                let mut f = Conjunctions::default();
                f.with.insert(with);
                ors.disjunctions.push(f);
            }

            for without in conj.without.iter() {
                let mut f = Conjunctions::default();
                f.without.insert(without);
                ors.disjunctions.push(f);
            }

            res.and(&ors);
        }

        *self = res;

        self
    }

    /// Performs an XOR (`⊻`) with `self` and `other` and stores the result in
    /// `self`. `self` is then returned for further chaining.
    pub fn xor(&mut self, other: &Self) -> &mut Self {
        let mut this = self.clone();
        let mut other = other.clone();

        // A ⊻ B ≡ (A ∧ ¬B) ∨ (B ∧ ¬A)
        self.and(other.clone().not()).or(other.and(this.not()))
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
        self.disjunctions.iter().all(|conj| {
            other
                .disjunctions
                .iter()
                .all(|other_conj| conj.is_disjoint(other_conj))
        })
    }

    pub fn clear_access(&mut self) {
        self.access.clear();
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
pub enum Access {
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
struct Conjunctions<T> {
    with: BitSet<T>,
    without: BitSet<T>,
}

impl<T: BitSetIndex> Conjunctions<T> {
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

impl<T: BitSetIndex + fmt::Debug> fmt::Debug for Conjunctions<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("AccessFilters")
            .field("with", &self.with)
            .field("without", &self.without)
            .finish()
    }
}

impl<T> Clone for Conjunctions<T> {
    fn clone(&self) -> Self {
        Self {
            with: self.with.clone(),
            without: self.without.clone(),
        }
    }
}

impl<T> Default for Conjunctions<T> {
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
    fn t0() {
        let expr = AccessExpr::with(0usize, Access::ReadWrite);
        let expr2 = expr.clone();
        let expr3 = AccessExpr::<usize>::with(0usize, Access::Read);

        assert!(!expr.is_compatible(&expr2));
        assert!(!expr.is_compatible(&expr3));
    }

    #[test]
    fn t1() {
        let mut expr = AccessExpr::with(0, Access::Read);
        expr.and(&AccessExpr::with(1, Access::ReadWrite));

        let mut expr2 = AccessExpr::with(0, Access::Read);
        expr2.and(&AccessExpr::with(1, Access::None));
        expr2.and(&AccessExpr::with(2, Access::ReadWrite));

        assert!(expr.is_compatible(&expr2));
    }

    #[test]
    fn t2() {
        let mut expr = AccessExpr::<usize>::with(0, Access::ReadWrite);
        expr.and(&AccessExpr::with(1, Access::None));

        let mut expr2 = AccessExpr::<usize>::with(0, Access::ReadWrite);
        expr2.and(&AccessExpr::without(1));

        assert!(expr.is_compatible(&expr2));
    }

    #[test]
    fn t3() {
        let mut expr = AccessExpr::<usize>::with(0, Access::ReadWrite);
        expr.and(&AccessExpr::with(1, Access::None));
        expr.and(&AccessExpr::without(1));

        let expr2 = AccessExpr::with(0, Access::ReadWrite);

        assert!(expr.is_compatible(&expr2));
    }

    #[test]
    fn negate_true() {
        let mut expr = AccessExpr::<usize>::one();

        expr.not();

        assert!(expr.disjunctions.is_empty());
    }

    #[test]
    fn negate_false() {
        let mut expr = AccessExpr::<usize>::zero();

        expr.not();

        assert!(expr.disjunctions[0].with.is_empty() && expr.disjunctions[0].without.is_empty());
    }
}
