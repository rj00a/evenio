use core::cmp::Ordering;

use super::Access;
use crate::component::ComponentIdx;
use crate::map::IndexSet;

/// An expressions describing the components accessed by a query.
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
    pub fn new() -> Self {
        Self::default() // TODO: make this consistent with ArchetypeFilter
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

        for left in &self.cases {
            for right in &rhs.cases {
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
                                    case.push((left_idx, left_access));
                                    il += 1;
                                }
                                Ordering::Equal => {
                                    use CaseAccess::*;
                                    il += 1;
                                    ir += 1;
                                    let combined_access = match (left_access, right_access) {
                                        (Conflict, _)
                                        | (_, Conflict)
                                        | (Read, ReadWrite)
                                        | (ReadWrite, Read)
                                        | (ReadWrite, ReadWrite) => Conflict,
                                        (With, Read) | (Read, With) | (Read, Read) => Read,
                                        (With, ReadWrite) | (ReadWrite, With) => ReadWrite,
                                        (With, With) => With,
                                        (Not, Not) => Not,
                                        (Not, With | Read | ReadWrite)
                                        // Case is impossible, so delete it.
                                        | (With | Read | ReadWrite, Not) => continue,
                                    };
                                    case.push((left_idx, combined_access));
                                }
                                Ordering::Greater => {
                                    case.push((right_idx, right_access));
                                    ir += 1;
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
            .fold(Self::new(), |acc, item| acc.and(&item))
    }

    pub fn or(&self, rhs: &Self) -> Self {
        Self {
            cases: self
                .cases
                .iter()
                .cloned()
                .chain(rhs.cases.iter().cloned())
                .chain(self.and(&rhs).cases)
                .collect(),
        }
    }

    pub fn xor(&self, rhs: &Self) -> Self {
        let left = self.and(&rhs.not());
        let right = rhs.and(&self.not());

        Self {
            cases: left.cases.into_iter().chain(right.cases).collect(),
        }
    }

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
}

impl Default for ComponentAccess {
    fn default() -> Self {
        Self {
            cases: vec![vec![]],
        }
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

        panic!(
            "{:?}",
            a(ReadWrite).and(&b(None)).or(&a(Read).and(&b(None).not()))
        );

        check(a(Read).and(&a(Read)), []);
        check(a(ReadWrite).and(&a(ReadWrite)), [A]);
        check(a(Read).and(&a(ReadWrite)).and(&a(None)), [A]);
        check(
            a(ReadWrite).and(&b(None)).or(&a(Read).and(&b(None).not())),
            [],
        );

        // panic!("{:?}", a(Read).and(&b(Read)).xor(&b(Read).and(&c(Read))));
    }
}
