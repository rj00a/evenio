use core::cmp::Ordering;

use super::Access;
use crate::component::ComponentIdx;
use crate::map::IndexSet;

/// An expressions describing the components accessed by a query.
#[must_use]
#[derive(Clone, Debug)]
pub struct ComponentAccess {
    /// The set of cases that need to be considered for access checking.
    ///
    /// If any of the cases end up with a [`CaseAccess::Conflict`], then we know
    /// the query is invalid.
    ///
    /// Example: The query `(Or<&A, &mut B>, &B)` has three cases:
    /// 1. Read `A` and read `B`.
    /// 2. Conflict in `B`.
    /// 3. Read `A`, Conflict in `B`.
    ///
    /// Since cases (2) and (3) have conflicts, we know the query is invalid.
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
                                    dbg!(left_access, right_access);
                                    il += 1;
                                    ir += 1;
                                    use CaseAccess::*;
                                    let combined_access = match (left_access, right_access) {
                                        (With, Read) | (Read, With) | (Read, Read) => Read,
                                        (With, ReadWrite) | (ReadWrite, With) => ReadWrite,
                                        (With, With) => With,
                                        (Not, Not) => Not,
                                        (Not, With | Read | ReadWrite | Conflict)
                                        | (With | Read | ReadWrite | Conflict, Not) => {
                                            // Skip this case since both accessing and not accessing
                                            // the same component is impossible.
                                            continue 'next_case;
                                        }
                                        (Conflict, With | Read | ReadWrite)
                                        | (With | Read | ReadWrite, Conflict)
                                        | (Conflict, Conflict)
                                        | (Read, ReadWrite)
                                        | (ReadWrite, Read)
                                        | (ReadWrite, ReadWrite) => Conflict,
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

    pub fn or(&self, rhs: &Self) -> Self {
        Self {
            cases: self.cases.iter().chain(rhs.cases.iter()).cloned().collect(),
        }
    }

    pub fn not(&self) -> Self {
        // Apply De Morgan's laws.
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
        let mut res = IndexSet::default();

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
        self.cases
            .iter()
            .any(|case| case.iter().all(|(idx, _)| f(*idx)))
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
    const D: ComponentIdx = ComponentIdx(2);

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

    fn d(access: Access) -> Ca {
        Ca::var(D, access)
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
    }
}
