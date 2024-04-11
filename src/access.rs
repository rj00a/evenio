//! Data access checking.

#[cfg(not(feature = "std"))]
use alloc::{vec, vec::Vec};
use core::cmp::Ordering;

use ahash::RandomState;

use crate::component::ComponentIdx;
use crate::map::IndexSet;

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
    /// Produces the access which is the superset of `self` and `other`. If the
    /// two accesses are incompatible with each other, then `None` is returned
    /// instead.
    ///
    /// # Examples
    ///
    /// ```
    /// use evenio::access::Access;
    ///
    /// assert_eq!(Access::Read.join(Access::Read), Some(Access::Read));
    /// assert_eq!(Access::Read.join(Access::ReadWrite), None);
    /// assert_eq!(
    ///     Access::ReadWrite.join(Access::None),
    ///     Some(Access::ReadWrite)
    /// );
    /// ```
    #[must_use]
    pub const fn join(self, other: Self) -> Option<Access> {
        match (self, other) {
            (Access::None, Access::None) => Some(Access::None),
            (Access::Read | Access::ReadWrite, Access::None) => Some(self),
            (Access::None, Access::Read | Access::ReadWrite) => Some(other),
            (Access::Read, Access::Read) => Some(Access::Read),
            (Access::Read | Access::ReadWrite, Access::ReadWrite)
            | (Access::ReadWrite, Access::Read) => None,
        }
    }

    /// Shorthand for `self.join(other).is_some()`. See [`join`] for more
    /// information.
    ///
    /// [`join`]: Self::join
    ///
    /// # Examples
    ///
    /// ```
    /// use evenio::access::Access::{None, Read, ReadWrite};
    ///
    /// assert!(Read.is_compatible(Read));
    /// assert!(!Read.is_compatible(ReadWrite));
    /// assert!(ReadWrite.is_compatible(None));
    /// ```
    #[must_use]
    pub const fn is_compatible(self, other: Self) -> bool {
        self.join(other).is_some()
    }
}

/// An expression describing the components accessed by a query.
///
/// This is used for checking that queries don't break aliasing rules, as well
/// as determining which archetypes are matched by a query.
#[must_use]
#[derive(Clone, Debug)]
pub struct ComponentAccess {
    /// The set of cases that need to be considered for access checking.
    ///
    /// If any of the cases end up with a [`CaseAccess::Conflict`], then we know
    /// the query is invalid.
    ///
    /// Example: The query `Or<&A, &mut A>` has three cases.
    /// 1. `&A` left branch.
    /// 2. `&mut A` right branch.
    /// 3. `(&A, &mut A)` both branches. `&A` and `&mut A` are merged together
    ///    to form a conflict in `A`.
    ///
    /// Since case (3) has a conflict, we know the whole query is invalid.
    ///
    /// This vec can be viewed as the "lists of lists" needed for
    /// [Disjunctive Normal Form][dnf], but with some necessary modifications in
    /// order to track access conflicts.
    ///
    /// [dnf]: https://en.wikipedia.org/wiki/Disjunctive_normal_form
    cases: Vec<Case>,
}

/// Association list from component to access. Sorted in ascending order by
/// [`ComponentIdx`].
type Case = Vec<(ComponentIdx, CaseAccess)>;

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
enum CaseAccess {
    /// Archetype must have the component, but the component is not read or
    /// written.
    With,
    /// Component is read.
    Read,
    /// Component is read and/or written.
    ReadWrite,
    /// Archetype must not have the component.
    Not,
    /// Access to component violates mutable aliasing.
    Conflict,
}

impl ComponentAccess {
    /// Create a new `ComponentAccess` which matches all archetypes but has no
    /// access.
    pub fn new_true() -> Self {
        Self {
            cases: vec![vec![]],
        }
    }

    /// Create a new `ComponentAccess` which matches no archetypes and has no
    /// access.
    pub fn new_false() -> Self {
        Self { cases: vec![] }
    }

    /// Create a new `ComponentAccess` which accesses a single component.
    pub fn var(idx: ComponentIdx, access: Access) -> Self {
        Self {
            cases: vec![vec![(
                idx,
                match access {
                    Access::None => CaseAccess::With,
                    Access::Read => CaseAccess::Read,
                    Access::ReadWrite => CaseAccess::ReadWrite,
                },
            )]],
        }
    }

    /// Logically AND two access expressions together.
    pub fn and(&self, rhs: &Self) -> Self {
        let mut cases = vec![];

        for right in &rhs.cases {
            'next_case: for left in &self.cases {
                let mut case = vec![];
                let mut il = 0;
                let mut ir = 0;

                loop {
                    match (left.get(il), right.get(ir)) {
                        (None, None) => break,
                        (None, Some(_)) => {
                            case.extend(right[ir..].iter().copied());
                            break;
                        }
                        (Some(_), None) => {
                            case.extend(left[il..].iter().copied());
                            break;
                        }
                        (Some(&(left_idx, left_access)), Some(&(right_idx, right_access))) => {
                            match left_idx.cmp(&right_idx) {
                                Ordering::Less => {
                                    il += 1;
                                    case.push((left_idx, left_access));
                                }
                                Ordering::Equal => {
                                    il += 1;
                                    ir += 1;
                                    use CaseAccess::*;
                                    let combined_access = match (left_access, right_access) {
                                        (With | Read, Read) | (Read, With) => Read,
                                        (With, ReadWrite) | (ReadWrite, With) => ReadWrite,
                                        (With, With) => With,
                                        (Not, Not) => Not,
                                        (Not, With | Read | ReadWrite | Conflict)
                                        | (With | Read | ReadWrite | Conflict, Not) => {
                                            // Skip this case since both accessing and not accessing
                                            // the same component is impossible.
                                            continue 'next_case;
                                        }
                                        (Conflict, With | Read | ReadWrite | Conflict)
                                        | (With | Read | ReadWrite, Conflict)
                                        | (Read | ReadWrite, ReadWrite)
                                        | (ReadWrite, Read) => Conflict,
                                    };
                                    case.push((left_idx, combined_access));
                                }
                                Ordering::Greater => {
                                    ir += 1;
                                    case.push((right_idx, right_access));
                                }
                            }
                        }
                    }
                }

                cases.push(case);
            }
        }

        Self { cases }
    }

    /// Logically OR two access expressions together.
    pub fn or(&self, rhs: &Self) -> Self {
        Self {
            cases: self.cases.iter().chain(rhs.cases.iter()).cloned().collect(),
        }
    }

    /// Inverts the accesses according to De Morgan's laws.
    ///
    /// This that this is a lossy operation because read/write information is
    /// lost.
    pub fn not(&self) -> Self {
        self.cases
            .iter()
            .map(|case| Self {
                cases: case
                    .iter()
                    .map(|&(idx, access)| {
                        let new_access = match access {
                            CaseAccess::With => CaseAccess::Not,
                            CaseAccess::Read => CaseAccess::Not,
                            CaseAccess::ReadWrite => CaseAccess::Not,
                            CaseAccess::Not => CaseAccess::With,
                            CaseAccess::Conflict => CaseAccess::Not,
                        };

                        vec![(idx, new_access)]
                    })
                    .collect(),
            })
            .fold(Self::new_true(), |acc, item| acc.and(&item))
    }

    /// Clears all component access without altering the matched archetypes.
    /// Equivalent to `self = self.not().not()`.
    pub fn clear_access(&mut self) {
        for case in &mut self.cases {
            for (_, access) in case {
                *access = match *access {
                    CaseAccess::With => CaseAccess::With,
                    CaseAccess::Read => CaseAccess::With,
                    CaseAccess::ReadWrite => CaseAccess::With,
                    CaseAccess::Not => CaseAccess::Not,
                    CaseAccess::Conflict => CaseAccess::With,
                }
            }
        }
    }

    /// Returns the set of all conflicting components.
    pub(crate) fn collect_conflicts(&self) -> IndexSet<ComponentIdx> {
        let mut res = IndexSet::with_hasher(RandomState::new());

        for case in &self.cases {
            for &(idx, access) in case {
                if access == CaseAccess::Conflict {
                    res.insert(idx);
                }
            }
        }

        res
    }

    pub(crate) fn matches_archetype<F>(&self, mut f: F) -> bool
    where
        F: FnMut(ComponentIdx) -> bool,
    {
        self.cases.iter().any(|case| {
            case.iter().all(|&(idx, access)| match access {
                CaseAccess::With => f(idx),
                CaseAccess::Read => f(idx),
                CaseAccess::ReadWrite => f(idx),
                CaseAccess::Not => !f(idx),
                CaseAccess::Conflict => f(idx),
            })
        })
    }
}

/// Equivalent to [`Self::new_false`].
impl Default for ComponentAccess {
    fn default() -> Self {
        Self::new_false()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    type Ca = ComponentAccess;

    const A: ComponentIdx = ComponentIdx(0);
    const B: ComponentIdx = ComponentIdx(1);
    const C: ComponentIdx = ComponentIdx(2);

    #[track_caller]
    fn check(ca: Ca, it: impl IntoIterator<Item = ComponentIdx>) {
        assert_eq!(
            ca.collect_conflicts(),
            IndexSet::<ComponentIdx>::from_iter(it)
        )
    }

    fn a(access: Access) -> Ca {
        Ca::var(A, access)
    }

    fn b(access: Access) -> Ca {
        Ca::var(B, access)
    }

    fn c(access: Access) -> Ca {
        Ca::var(C, access)
    }

    #[test]
    fn collect_conflicts() {
        use Access::*;

        check(a(Read).and(&a(Read)), []);
        check(a(ReadWrite).and(&a(ReadWrite)), [A]);
        check(a(Read).and(&a(ReadWrite)).and(&a(None)), [A]);

        check(
            a(ReadWrite).and(&b(None)).and(&a(Read).and(&b(None).not())),
            [],
        );

        // (Xor<(&A, &B), (&B, &C)>, &mut B)
        let left = a(Read).and(&b(Read));
        let right = b(Read).and(&c(Read));
        let xor = left.and(&right.not()).or(&right.and(&left.not()));
        check(xor.and(&b(ReadWrite)), [B]);
    }
}
