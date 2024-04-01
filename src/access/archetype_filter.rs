use core::cmp::Ordering;
use core::fmt;

use crate::bit_set::BitSet;
use crate::component::ComponentIdx;

#[derive(Clone, PartialEq, Eq, Default)]
pub struct ArchetypeFilter {
    /// A boolean formula in [algebraic normal form (ANF)](https://en.wikipedia.org/wiki/Algebraic_normal_form).
    ///
    /// This is an "XOR of ANDs". The outer list is sorted in ascending order.
    terms: Vec<BitSet<ComponentIdx>>,
}

impl ArchetypeFilter {
    /// Create a new filter matching no archetypes.
    pub fn new() -> Self {
        Self::default()
    }

    /// Create a new filter matching archetypes with the given component.
    pub fn var(idx: ComponentIdx) -> Self {
        let mut term = BitSet::new();
        term.insert(idx);

        Self { terms: vec![term] }
    }

    pub fn xor(&self, rhs: &Self) -> Self {
        let mut terms = vec![];

        let mut il = 0;
        let mut ir = 0;
        loop {
            match (self.terms.get(il), rhs.terms.get(ir)) {
                (None, None) => break,
                (None, Some(_)) => {
                    terms.extend(rhs.terms[ir..].iter().cloned());
                    break;
                }
                (Some(_), None) => {
                    terms.extend(self.terms[il..].iter().cloned());
                    break;
                }
                (Some(left), Some(right)) => match left.cmp(right) {
                    Ordering::Less => {
                        terms.push(left.clone());
                        il += 1;
                    }
                    Ordering::Equal => {
                        // Equal terms cancel out.
                        il += 1;
                        ir += 1;
                    }
                    Ordering::Greater => {
                        terms.push(right.clone());
                        ir += 1;
                    }
                },
            }
        }

        Self { terms }
    }

    pub fn not(&self) -> Self {
        self.xor(&Self::from(true))
    }

    pub fn and(&self, rhs: &Self) -> Self {
        let mut res = Self::new();

        let mut temp = Self {
            terms: vec![BitSet::new()],
        };

        // AND distributes over XOR.
        for right_term in &rhs.terms {
            for left_term in &self.terms {
                let term = &mut temp.terms[0];

                *term |= left_term;
                *term |= right_term;

                res = res.xor(&temp);
                temp.terms[0].clear();
            }
        }

        res
    }

    pub fn or(&self, rhs: &Self) -> Self {
        // a ∨ b = a ⊻ b ⊻ (a ∧ b)
        self.xor(rhs).xor(&self.and(rhs))
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
        self.terms
            .iter()
            .fold(false, |acc, term| acc ^ term.iter().all(|idx| get_var(idx)))
    }
}

/// Create a new archetype filter matching everything (`true`) or nothing
/// (`false`).
impl From<bool> for ArchetypeFilter {
    fn from(value: bool) -> Self {
        Self {
            terms: if value { vec![BitSet::new()] } else { vec![] },
        }
    }
}

impl fmt::Debug for ArchetypeFilter {
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

                        write!(f, "C{}", var.0)?;
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

#[cfg(test)]
mod tests {
    use super::*;

    fn vars() -> [ArchetypeFilter; 4] {
        [
            ArchetypeFilter::from(true),
            ArchetypeFilter::var(ComponentIdx(0)),
            ArchetypeFilter::var(ComponentIdx(1)),
            ArchetypeFilter::var(ComponentIdx(2)),
        ]
    }

    /// (1 ⊕ x) ⊕ (1 ⊕ x ⊕ y) = y
    #[test]
    fn xor() {
        let [one, x, y, _] = &vars();

        let lhs = one.xor(x).xor(&one.xor(x).xor(y));
        let rhs = y;
        assert_eq!(lhs, *rhs);
    }

    /// ¬(1 ⊕ x ⊕ y) = x ⊕ y
    #[test]
    fn not() {
        let [one, x, y, _] = &vars();

        let lhs = one.xor(x).xor(y).not();
        let rhs = x.xor(y);
        assert_eq!(lhs, rhs);
    }

    /// (1 ⊕ x)(1 ⊕ x ⊕ y) = 1 ⊕ x ⊕ y ⊕ xy
    #[test]
    fn and() {
        let [one, x, y, _] = &vars();

        let lhs = one.xor(x).and(&one.xor(x).xor(y));
        let rhs = one.xor(x).xor(y).xor(&x.and(y));
        assert_eq!(lhs, rhs);
    }

    /// (1 ⊕ x) + (1 ⊕ x ⊕ y) = 1 ⊕ x ⊕ xy
    #[test]
    fn or() {
        let [one, x, y, _] = &vars();

        let lhs = one.xor(x).or(&one.xor(x).xor(y));
        let rhs = one.xor(x).xor(&x.and(y));
        assert_eq!(lhs, rhs);
    }

    /// (xz)(yz) = xyz
    #[test]
    fn and_vars() {
        let [_, x, y, z] = &vars();

        let lhs = x.and(z).and(&y.and(z));
        let rhs = x.and(y).and(z);
        assert_eq!(lhs, rhs);
    }
}
