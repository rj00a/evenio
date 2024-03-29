//! Data access checking.

use core::cmp::Ordering;
use core::fmt;
use std::collections::HashSet;

// use crate::component::ComponentIdx;

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct ComponentIdx(pub u32);

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
}

#[derive(Clone)]
pub struct ComponentAccessExpr {
    /// A propositional logic formula in [Algebraic Normal Form (ANF)](https://en.wikipedia.org/wiki/Algebraic_normal_form).
    /// This is an XOR of ANDs, e.g. `1 ⊕ x ⊕ y ⊕ xy`.
    ///
    /// All vectors are sorted and every variable (component index) is paired
    /// with a [`VarAccess`].
    terms: Vec<Vec<Variable>>,
}

impl ComponentAccessExpr {
    pub fn new(b: bool) -> Self {
        Self {
            terms: if b { vec![vec![]] } else { vec![] },
        }
    }

    pub fn var(idx: ComponentIdx, access: Access) -> Self {
        Self {
            terms: vec![vec![Variable {
                idx,
                access: VarAccess::from(access),
            }]],
        }
    }

    pub fn xor(&self, other: &Self) -> Self {
        let mut new = vec![];

        let mut left = 0;
        let mut right = 0;

        loop {
            match (self.terms.get(left), other.terms.get(right)) {
                (None, None) => break,
                (None, Some(r)) => {
                    new.push(r.clone());
                    right += 1;
                }
                (Some(l), None) => {
                    new.push(l.clone());
                    left += 1;
                }
                (Some(l), Some(r)) => match l.cmp(r) {
                    Ordering::Less => {
                        new.push(l.clone());
                        left += 1;
                    }
                    Ordering::Equal => {
                        // Equal terms cancel out.
                        left += 1;
                        right += 1;
                    }
                    Ordering::Greater => {
                        new.push(r.clone());
                        right += 1;
                    }
                },
            }
        }

        Self { terms: new }
    }

    pub fn not(&self) -> Self {
        // ¬a = 1 ⊕ a
        self.xor(&Self::new(true))
    }

    pub fn clear_access(&mut self) {
        for term in &mut self.terms {
            for var in term {
                var.access = VarAccess::None;
            }
        }
    }

    pub fn and(&self, other: &Self) -> Self {
        let mut res = Self::new(false);

        let mut rhs = Self {
            terms: vec![vec![]],
        };

        // AND distributes over XOR.
        for right_term in &other.terms {
            for left_term in &self.terms {
                let mut left = 0;
                let mut right = 0;

                let term = &mut rhs.terms[0];

                loop {
                    match (left_term.get(left), right_term.get(right)) {
                        (None, None) => break,
                        (None, Some(r)) => {
                            term.push(r.clone());
                            right += 1;
                        }
                        (Some(l), None) => {
                            term.push(l.clone());
                            left += 1;
                        }
                        (Some(l), Some(r)) => match l.cmp(r) {
                            Ordering::Less => {
                                term.push(l.clone());
                                left += 1;
                            }
                            Ordering::Equal => {
                                term.push(Variable {
                                    idx: l.idx,
                                    access: l.access.join(r.access),
                                });

                                left += 1;
                                right += 1;
                            }
                            Ordering::Greater => {
                                term.push(r.clone());
                                right += 1;
                            }
                        },
                    }
                }

                res = res.xor(&rhs);
                rhs.terms[0].clear();
            }
        }

        res
    }

    pub fn or(&self, other: &Self) -> Self {
        // a ∨ b = a ⊕ b ⊕ ab
        self.xor(other).xor(&self.and(other))
    }

    pub fn is_true(&self) -> bool {
        self.terms.len() == 1 && self.terms[0].is_empty()
    }

    pub fn is_false(&self) -> bool {
        self.terms.is_empty()
    }

    /// Evaluate the component access expression. `get_var` provides the values
    /// of the variables in the expression.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use evenio::bool_expr::BoolExpr;
    ///
    /// const A: u32 = 0;
    /// const B: u32 = 1;
    ///
    /// let expr = BoolExpr::var(A).xor(&BoolExpr::var(B));
    ///
    /// let get_var = |a, b| {
    ///     move |var| match var {
    ///         A => a,
    ///         B => b,
    ///         _ => false,
    ///     }
    /// };
    ///
    /// assert_eq!(expr.eval(get_var(false, false)), false);
    /// assert_eq!(expr.eval(get_var(true, false)), true);
    /// assert_eq!(expr.eval(get_var(false, true)), true);
    /// assert_eq!(expr.eval(get_var(true, true)), false);
    /// ```
    pub fn eval<F>(&self, mut get_var: F) -> bool
    where
        F: FnMut(ComponentIdx) -> bool,
    {
        self.terms.iter().fold(false, |acc, term| {
            acc ^ term.iter().all(|var| get_var(var.idx))
        })
    }

    pub fn collect_conflicts(&self) -> HashSet<ComponentIdx> {
        self.terms
            .iter()
            .flat_map(|term| {
                term.iter()
                    .filter(|v| v.access == VarAccess::Conflict)
                    .map(|v| v.idx)
            })
            .collect()
    }

    pub fn has_conflicts(&self) -> bool {
        self.terms
            .iter()
            .any(|term| term.iter().any(|v| v.access == VarAccess::Conflict))
    }
}

impl fmt::Debug for ComponentAccessExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.terms.is_empty() {
            write!(f, "⊥")?;
        } else {
            let mut first = true;

            for term in &self.terms {
                if !first {
                    write!(f, " ⊻ ")?;
                }
                first = false;

                if term.is_empty() {
                    write!(f, "⊤")?;
                } else {
                    let mut first = true;

                    if term.len() > 1 {
                        write!(f, "(")?;
                    }

                    for var in term {
                        if !first {
                            write!(f, " ∧ ")?;
                        }
                        first = false;

                        match var.access {
                            VarAccess::None => write!(f, "{}", var.idx.0),
                            VarAccess::Read => write!(f, "&{}", var.idx.0),
                            VarAccess::ReadWrite => write!(f, "&mut {}", var.idx.0),
                            VarAccess::Conflict => write!(f, "&bad {}", var.idx.0),
                        }?;
                    }

                    if term.len() > 1 {
                        write!(f, ")")?;
                    }
                }
            }
        }

        Ok(())
    }
}

#[derive(Clone, Debug)]
struct Variable {
    idx: ComponentIdx, 
    access: VarAccess,
    archetype_ignore: bool,
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
enum VarAccess {
    None,
    Read,
    ReadWrite,
    Conflict,
}

impl VarAccess {
    const fn join(self, other: Self) -> Self {
        match (self, other) {
            (Self::Conflict, _) => Self::Conflict,
            (_, Self::Conflict) => Self::Conflict,
            (this, Self::None) => this,
            (Self::None, other) => other,
            (Self::Read, Self::Read) => Self::Read,
            _ => Self::Conflict,
        }
    }
}

impl From<Access> for VarAccess {
    fn from(value: Access) -> Self {
        match value {
            Access::None => Self::None,
            Access::Read => Self::Read,
            Access::ReadWrite => Self::ReadWrite,
        }
    }
}

impl PartialEq for Variable {
    fn eq(&self, other: &Self) -> bool {
        self.idx == other.idx
    }
}

impl Eq for Variable {}

impl PartialOrd for Variable {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Variable {
    fn cmp(&self, other: &Self) -> Ordering {
        self.idx.cmp(&other.idx)
    }
}

/*
/// Map from `T` to [`Access`] with sparse index keys. All keys map to
/// [`Access::None`] by default.
pub struct AccessMap<T> {
    read: BitSet<T>,
    write: BitSet<T>,
}

impl<T> AccessMap<T> {
    /// Creates an empty access map.
    pub fn new() -> Self {
        Self {
            read: BitSet::new(),
            write: BitSet::new(),
        }
    }

    /// Gets the access for `key`.
    pub fn get(&self, key: T) -> Access
    where
        T: SparseIndex,
    {
        match (self.read.contains(key), self.write.contains(key)) {
            (true, true) => Access::ReadWrite,
            (true, false) => Access::Read,
            (false, true) => unreachable!("read-write implies read"),
            (false, false) => Access::None,
        }
    }

    /// Sets the access for `key`.
    pub fn set(&mut self, key: T, access: Access)
    where
        T: SparseIndex,
    {
        match access {
            Access::None => {
                self.read.remove(key);
                self.write.remove(key);
            }
            Access::Read => {
                self.read.insert(key);
                self.write.remove(key);
            }
            Access::ReadWrite => {
                self.read.insert(key);
                self.write.insert(key);
            }
        }
    }

    /// Clears the access map. All keys will map to [`Access::None`].
    pub fn clear(&mut self) {
        self.read.clear();
        self.write.clear();
    }

    /// Returns whether all values in `self` can be active at the same time as
    /// all values in `other`.
    pub fn is_compatible(&self, other: &Self) -> bool {
        self.read.is_disjoint(&other.write) && self.write.is_disjoint(&other.read)
    }

    /// Computes the union between `self` and `other` and assigns the result to
    /// `self`.
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

/// The combination of a [`BoolExpr`] with an [`AccessMap`].
///
/// This is capable of precisely describing the components accessed by a query,
/// e.g. `(&A, &mut B)`.
#[derive(Clone, Debug)]
pub struct ComponentAccessExpr {
    /// The boolean expression part.
    pub expr: BoolExpr<ComponentIdx>,
    /// The access map part.
    pub access: AccessMap<ComponentIdx>,
}

#[allow(clippy::should_implement_trait)]
impl ComponentAccessExpr {
    /// Creates a new component access expr corresponding to `true` or `false`.
    /// The access map is initialized empty.
    pub fn new(b: bool) -> Self {
        Self {
            expr: BoolExpr::new(b),
            access: AccessMap::new(),
        }
    }

    /// Creates a new component access expr which accesses a single component,
    /// e.g. `&A` or `&mut A`.
    pub fn with(component: ComponentIdx, access: Access) -> Self {
        let mut map = AccessMap::new();
        map.set(component, access);

        Self {
            expr: BoolExpr::var(component),
            access: map,
        }
    }

    /// Creates a new component access expr which does not access a single
    /// component, e.g. `Not<A>`.
    pub fn without(component: ComponentIdx) -> Self {
        Self {
            expr: BoolExpr::not_var(component),
            access: AccessMap::new(),
        }
    }

    /// Checks if `self` can be active as the same time as `other` without
    /// causing access conflicts.
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

    /// ANDs two access exprs together. Returns `Err` if the two exprs are
    /// incompatible.
    pub fn and(mut self, other: &Self) -> Result<Self, Self> {
        if !self.is_compatible(other) {
            return Err(self);
        }

        self.access.union_assign(&other.access);
        self.expr = self.expr.and(&other.expr);
        Ok(self)
    }

    /// ORs two access exprs together. Returns `Err` if the two exprs are
    /// incompatible.
    pub fn or(mut self, other: &Self) -> Result<Self, Self> {
        if !self.is_compatible(other) {
            return Err(self);
        }

        self.access.union_assign(&other.access);
        self.expr = self.expr.or(&other.expr);
        Ok(self)
    }

    /// Performs a logical NOT on this expr. The access map is cleared.
    pub fn not(mut self) -> Self {
        self.access.clear();

        self.expr = self.expr.not();
        self
    }

    /// XORs two access exprs together.
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
*/

#[cfg(test)]
mod tests {
    use super::*;

    type Cae = ComponentAccessExpr;
    const X: ComponentIdx = ComponentIdx(0);
    const Y: ComponentIdx = ComponentIdx(1);
    const Z: ComponentIdx = ComponentIdx(2);
    use Access::*;

    /// (1 ⊕ x) ⊕ (1 ⊕ x ⊕ y) = y
    #[test]
    fn xor() {
        let left = Cae::new(true).xor(&Cae::var(X, None)).xor(
            &Cae::new(true)
                .xor(&Cae::var(X, None))
                .xor(&Cae::var(Y, None)),
        );

        let right = Cae::var(Y, None);

        assert_eq!(left.terms, right.terms);
    }

    /// ¬(1 ⊕ x ⊕ y) = x ⊕ y
    #[test]
    fn not() {
        let left = Cae::new(true)
            .xor(&Cae::var(X, None))
            .xor(&Cae::var(Y, None))
            .not();

        let right = Cae::var(X, None).xor(&Cae::var(Y, None));

        assert_eq!(left.terms, right.terms);
    }

    /// (1 ⊕ x)(1 ⊕ x ⊕ y) = 1 ⊕ x ⊕ y ⊕ xy
    #[test]
    fn and() {
        let left = Cae::new(true)
            .xor(&Cae::var(X, None))
            .and(&Cae::new(true).xor(&Cae::var(X, None).xor(&Cae::var(Y, None))));

        let right = Cae::new(true)
            .xor(&Cae::var(X, None))
            .xor(&Cae::var(Y, None))
            .xor(&Cae::var(X, None).and(&Cae::var(Y, None)));

        assert_eq!(left.terms, right.terms, "{left:?} ... {right:?}");
    }

    /// (1 ⊕ x) + (1 ⊕ x ⊕ y) = 1 ⊕ x ⊕ xy
    #[test]
    fn or() {
        let left = Cae::new(true)
            .xor(&Cae::var(X, None))
            .or(&Cae::new(true).xor(&Cae::var(X, None).xor(&Cae::var(Y, None))));

        let right =
            &Cae::new(true).xor(&Cae::var(X, None).xor(&Cae::var(X, None).and(&Cae::var(Y, None))));

        assert_eq!(left.terms, right.terms, "{left:?} ... {right:?}");
    }

    /// (xz)(yz) = xyz
    #[test]
    fn and_vars() {
        let left = Cae::var(X, None)
            .and(&Cae::var(Z, None))
            .and(&Cae::var(Y, None).and(&Cae::var(Z, None)));

        let right = Cae::var(X, None)
            .and(&Cae::var(Y, None))
            .and(&Cae::var(Z, None));

        assert_eq!(left.terms, right.terms, "{left:?} ... {right:?}");
    }

    #[test]
    fn is_true() {
        let expr = Cae::new(true);
        assert!(expr.is_true());
    }

    #[test]
    fn is_false() {
        let expr = Cae::new(false);
        assert!(expr.is_false());
    }

    #[test]
    fn conflicts() {
        assert!(Cae::var(X, Read)
            .and(&Cae::var(X, ReadWrite))
            .has_conflicts());

        assert!(!Cae::var(X, Read).and(&Cae::var(X, Read)).has_conflicts());

        assert!(!Cae::var(X, None)
            .and(&Cae::var(X, ReadWrite))
            .has_conflicts());
    }

    #[test]
    fn or_true() {
        assert!(Cae::var(X, ReadWrite).or(&Cae::new(true)).is_true());
    }

    #[test]
    fn blah() {
        // (&mut X and Y) or (&X and not Y)

        let expr = Cae::var(X, ReadWrite)
            .and(&Cae::var(Y, ReadWrite))
            .or(&Cae::var(X, Read).and(&Cae::var(Y, None).not()));

        assert_eq!(expr.collect_conflicts(), HashSet::new());

        // panic!("{expr:?}");
    }

    // B xor Not<B> // 

    // XY or (X and not Y)
    // - Rewrite NOT
    // XY or (X and (1 xor Y))
    // - Rewrite AND
    // XY or (X xor XY)
    // - Rewrite OR
    // XY xor (X xor XY) xor (XY and (X xor XY))
    // - Rewrite AND
    // XY xor (X xor XY) xor (XYX xor XYXY)
    // - Simplify AND
    // XY xor (X xor XY) xor (XY xor XY)
    // - Delete cancelled terms
    // XY xor X xor XY
    // - Delete more cancelled terms
    // X

    // (&mut X and Y) or (&X and not Y)
    // - Rewrite NOT
    // (&mut X and Y) or (&X and (1 xor Y))
    // - Rewrite AND
    // (&mut X and Y) or (&X xor (&X and Y))
    // - Rewrite OR
    // (&mut X and Y) xor (&X xor (&X and Y)) xor ((&mut X and Y) and (&X xor
    // (&X and Y)))
    // - Rewrite AND
    // (&mut X and Y) xor (&X xor (&X and Y)) xor (&mut X and Y and &X) xor
    // (&mut X and Y and &X and Y)
    // - Simplify AND
    // (&mut X and Y) xor (&X xor (&X and Y)) xor (&bad X and Y) xor (&bad X and
    // Y)
    // - Delete cancelled terms
    // (&mut X and Y) xor &X xor (&X and Y)
    // - Delete more cancelled terms
    // &X

    // Y
    // - NOT
    // (1 xor Y)
    // - AND
    // (1 xor Y) and &X => &X xor XY
    // - OR
    // (X xor XY) or XY
    // - AND
    // XY xor (X xor XY) xor ((X xor XY) and XY)
    // - AND
    // XY xor (X xor XY)
}

fn main() {}
