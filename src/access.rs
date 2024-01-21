use core::cmp::Ordering;
use core::fmt;

use crate::bit_set::BitSet;
use crate::bool_expr::BoolExpr;
use crate::component::ComponentIdx;
use crate::sparse::SparseIndex;

/// Describes how a particular piece of data is accessed. Used to prevent
/// aliased mutabliity.
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
    /// Can `self` and `other` be active at the same time without causing
    /// aliased mutability?
    ///
    /// This operation is symmetric.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use evenio::access::Access::*;
    ///
    /// assert!(Read.is_compatible(Read));
    /// assert!(!Read.is_compatible(ReadWrite));
    /// assert!(ReadWrite.is_compatible(None));
    /// ```
    #[must_use]
    pub const fn is_compatible(self, other: Self) -> bool {
        matches!(
            (self, other),
            (Access::None, _)
                | (Access::Read, Access::None | Access::Read)
                | (Access::ReadWrite, Access::None)
        )
    }

    /// Sets `self` equal to `other` if `self` is [compatible] with `other`.
    /// Returns whether or not the assignment occurred.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use evenio::access::Access;
    ///
    /// let mut access = Access::None;
    ///
    /// assert!(access.set_if_compatible(Access::ReadWrite));
    /// assert_eq!(access, Access::ReadWrite);
    /// ```
    ///
    /// [compatible](Self::is_compatible)
    #[must_use]
    pub fn set_if_compatible(&mut self, other: Self) -> bool {
        if self.is_compatible(other) {
            *self = other;
            true
        } else {
            false
        }
    }
}

/// Map from `T` to [`Access`] with sparse index keys. Absent keys implicitly
/// map to [`Access::None`].
pub struct AccessMap<T> {
    read: BitSet<T>,
    write: BitSet<T>,
}

impl<T> AccessMap<T> {
    pub fn new() -> Self {
        Self {
            read: BitSet::new(),
            write: BitSet::new(),
        }
    }

    pub fn get(&self, value: T) -> Access
    where
        T: SparseIndex,
    {
        match (self.read.contains(value), self.write.contains(value)) {
            (true, true) => Access::ReadWrite,
            (true, false) => Access::Read,
            (false, true) => unreachable!("read-write implies read"),
            (false, false) => Access::None,
        }
    }

    pub fn insert(&mut self, value: T, access: Access)
    where
        T: SparseIndex,
    {
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

    pub fn clear(&mut self) {
        self.read.clear();
        self.write.clear();
    }

    pub fn is_compatible(&self, other: &Self) -> bool {
        self.read.is_disjoint(&other.write) && self.write.is_disjoint(&other.read)
    }

    pub fn union_assign(&mut self, other: &Self) {
        self.read |= &other.read;
        self.write |= &other.write;
    }
}

impl<T> Default for AccessMap<T> {
    fn default() -> Self {
        Self::new()
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

impl<T: SparseIndex> Ord for AccessMap<T> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.read
            .cmp(&other.read)
            .then_with(|| self.write.cmp(&other.write))
    }
}

impl<T: SparseIndex> PartialOrd for AccessMap<T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl<T: SparseIndex> PartialEq for AccessMap<T> {
    fn eq(&self, other: &Self) -> bool {
        self.cmp(other).is_eq()
    }
}

impl<T: SparseIndex> Eq for AccessMap<T> {}

impl<T> fmt::Debug for AccessMap<T>
where
    T: SparseIndex + fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("AccessMap")
            .field("read", &self.read)
            .field("write", &self.write)
            .finish()
    }
}

#[derive(Clone, Debug)]
pub struct ComponentAccessExpr {
    pub expr: BoolExpr<ComponentIdx>,
    pub access: AccessMap<ComponentIdx>,
}

#[allow(clippy::should_implement_trait)]
impl ComponentAccessExpr {
    pub fn new(b: bool) -> Self {
        Self {
            expr: BoolExpr::new(b),
            access: AccessMap::new(),
        }
    }

    pub fn with(component: ComponentIdx, access: Access) -> Self {
        let mut map = AccessMap::new();
        map.insert(component, access);

        Self {
            expr: BoolExpr::with(component),
            access: map,
        }
    }

    pub fn without(component: ComponentIdx) -> Self {
        Self {
            expr: BoolExpr::without(component),
            access: AccessMap::new(),
        }
    }

    pub fn is_compatible(&self, other: &Self) -> bool {
        if self.access.is_compatible(&other.access) {
            return true;
        }

        // The component accesses of `self` and `other` are incompatible. However, if
        // the accesses don't overlap, then there's no incompatibility.
        //
        // For instance, `(&mut A, &B)` and `(&mut A, Not<&B>)` are incompatible with
        // respect to `A`, but are disjoint so there's no overlap.
        self.expr.is_disjoint(&other.expr)
    }

    pub fn and(mut self, other: &Self) -> Result<Self, Self> {
        if !self.is_compatible(other) {
            return Err(self);
        }

        self.access.union_assign(&other.access);
        self.expr = self.expr.and(&other.expr);
        Ok(self)
    }

    pub fn or(mut self, other: &Self) -> Result<Self, Self> {
        if !self.is_compatible(other) {
            return Err(self);
        }

        self.access.union_assign(&other.access);
        self.expr = self.expr.or(&other.expr);
        Ok(self)
    }

    pub fn not(mut self) -> Self {
        self.access.clear();

        self.expr = self.expr.not();
        self
    }

    pub fn xor(mut self, other: &Self) -> Self {
        self.access.union_assign(&other.access);
        self.expr = self.expr.xor(&other.expr);
        self
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    type Cae = ComponentAccessExpr;
    const A: ComponentIdx = ComponentIdx(0);
    const B: ComponentIdx = ComponentIdx(1);
    const C: ComponentIdx = ComponentIdx(2);

    #[test]
    fn t0() {
        let expr = Cae::with(A, Access::ReadWrite);
        let expr2 = expr.clone();
        let expr3 = Cae::with(A, Access::Read);

        assert!(!expr.is_compatible(&expr2));
        assert!(!expr.is_compatible(&expr3));
    }

    #[test]
    fn t1() {
        let expr = Cae::with(A, Access::Read)
            .and(&Cae::with(B, Access::ReadWrite))
            .unwrap();

        let expr2 = Cae::with(A, Access::Read)
            .and(&Cae::with(B, Access::None))
            .unwrap()
            .and(&Cae::with(C, Access::ReadWrite))
            .unwrap();

        assert!(expr.is_compatible(&expr2));
    }

    #[test]
    fn t2() {
        let expr = Cae::with(A, Access::ReadWrite)
            .and(&Cae::with(B, Access::None))
            .unwrap();

        let expr2 = Cae::with(A, Access::ReadWrite)
            .and(&Cae::without(B))
            .unwrap();

        assert!(expr.is_compatible(&expr2));
    }

    #[test]
    fn t3() {
        let expr = Cae::with(A, Access::ReadWrite)
            .and(&Cae::with(B, Access::None))
            .unwrap()
            .and(&Cae::without(C))
            .unwrap();

        let expr2 = Cae::with(A, Access::None);

        assert!(expr.is_compatible(&expr2));
    }
}
