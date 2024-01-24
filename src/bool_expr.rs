//! Boolean expressions.

use alloc::vec;
use alloc::vec::Vec;
use core::{fmt, mem};

use crate::bit_set::BitSet;
use crate::sparse::SparseIndex;

/// Represents an arbitrary boolean expression from boolean algebra. Values of
/// type `T` are used as the variables.
pub struct BoolExpr<T> {
    // The boolean expression in disjunctive normal form,
    // e.g. (A ∧ B ∧ ¬C) ∨ (D ∧ ¬E ∧ ¬F). This is an "OR of ANDs".
    ands: Vec<Ands<T>>,
}

struct Ands<T> {
    vars: BitSet<T>,
    negated_vars: BitSet<T>,
}

impl<T> Ands<T> {
    fn new() -> Self {
        Self {
            vars: BitSet::new(),
            negated_vars: BitSet::new(),
        }
    }
}

#[allow(clippy::should_implement_trait)]
impl<T> BoolExpr<T> {
    /// Creates an new expression of either `true` or `false`.
    pub fn new(b: bool) -> Self {
        Self {
            ands: if b { vec![Ands::new()] } else { vec![] },
        }
    }

    /// Create an expression from a single variable.
    pub fn var(value: T) -> Self
    where
        T: SparseIndex,
    {
        Self {
            ands: vec![Ands {
                vars: {
                    let mut vars = BitSet::new();
                    vars.insert(value);
                    vars
                },
                negated_vars: BitSet::new(),
            }],
        }
    }

    /// Create an expression from a single negated variable. Equivalent to
    /// `BoolExpr::var(value).not()`.
    pub fn not_var(value: T) -> Self
    where
        T: SparseIndex,
    {
        Self {
            ands: vec![Ands {
                vars: BitSet::new(),
                negated_vars: {
                    let mut negated_vars = BitSet::new();
                    negated_vars.insert(value);
                    negated_vars
                },
            }],
        }
    }

    /// Evaluate the boolean expression. `get_var` provides the values of the
    /// variables in the expression.
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
        T: SparseIndex,
        F: FnMut(T) -> bool,
    {
        'ands: for ands in &self.ands {
            for var in &ands.vars {
                if !get_var(var) {
                    continue 'ands;
                }
            }

            for var in &ands.negated_vars {
                if get_var(var) {
                    continue 'ands;
                }
            }

            return true;
        }

        false
    }

    /// AND two expressions together.
    #[must_use]
    pub fn and(mut self, other: &Self) -> Self
    where
        T: SparseIndex,
    {
        let mut res = Vec::new();
        for this in &self.ands {
            for other in &other.ands {
                let mut new_ands = this.clone();

                new_ands.vars |= &other.vars;
                new_ands.negated_vars |= &other.negated_vars;

                // Skip contradictions.
                if new_ands.vars.is_disjoint(&new_ands.negated_vars) {
                    res.push(new_ands);
                }
            }
        }

        self.ands = res;
        self
    }

    /// OR two expressions together.
    #[must_use]
    pub fn or(mut self, other: &Self) -> Self
    where
        T: SparseIndex,
    {
        self.ands.extend(other.ands.iter().cloned());
        self
    }

    /// Negates `self`.
    #[must_use]
    pub fn not(mut self) -> Self
    where
        T: SparseIndex,
    {
        let mut res = Self::new(true);

        // Apply De Morgan's laws.
        for mut ands in mem::take(&mut self.ands) {
            let mut ors = Self::new(false);

            mem::swap(&mut ands.vars, &mut ands.negated_vars);

            for var in &ands.vars {
                let mut a = Ands::new();
                a.vars.insert(var);
                ors.ands.push(a);
            }

            for negated_var in &ands.negated_vars {
                let mut a = Ands::new();
                a.negated_vars.insert(negated_var);
                ors.ands.push(a);
            }

            res = res.and(&ors);
        }

        res
    }

    /// XOR two expressions together.
    pub fn xor(self, other: &Self) -> Self
    where
        T: SparseIndex,
    {
        // A ⊻ B ≡ (A ∧ ¬B) ∨ (B ∧ ¬A)
        self.clone()
            .and(&other.clone().not())
            .or(&other.clone().and(&self.not()))
    }

    /// Determines if `self` and `other` are disjoint, i.e. if there is no
    /// combination of values the variables could have to make both expressions
    /// true at the same time.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use evenio::bool_expr::BoolExpr;
    ///
    /// // `A` is not disjoint with `B`
    /// assert!(!BoolExpr::var(A).is_disjoint(&BoolExpr::var(B)));
    ///
    /// // `A` is disjoint with `¬A`.
    /// assert!(BoolExpr::var(A).is_disjoint(&BoolExpr::not_var(A)));
    ///
    /// // `A ∧ ¬A` is disjoint with `B ∧ C`.
    /// let left = BoolExpr::var(A).and(&BoolExpr::not_var(A));
    /// let right = BoolExpr::var(C).and(&BoolExpr::var(D));
    /// assert!(left.is_disjoint(&right));
    ///
    /// const A: u32 = 0;
    /// const B: u32 = 1;
    /// const C: u32 = 2;
    /// const D: u32 = 3;
    /// ```
    pub fn is_disjoint(&self, other: &Self) -> bool {
        self.ands.iter().all(|this| {
            other.ands.iter().all(|other| {
                !this.vars.is_disjoint(&this.negated_vars)
                    || !other.vars.is_disjoint(&other.negated_vars)
                    || !this.vars.is_disjoint(&other.negated_vars)
                    || !other.vars.is_disjoint(&this.negated_vars)
            })
        })
    }
}

impl<T> Clone for BoolExpr<T> {
    fn clone(&self) -> Self {
        Self {
            ands: self.ands.clone(),
        }
    }
}

impl<T> Clone for Ands<T> {
    fn clone(&self) -> Self {
        Self {
            vars: self.vars.clone(),
            negated_vars: self.negated_vars.clone(),
        }
    }
}

impl<T> fmt::Debug for BoolExpr<T>
where
    T: SparseIndex + fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.ands.is_empty() {
            write!(f, "⊥")?;
        } else {
            let mut first = true;

            for ands in &self.ands {
                if !first {
                    write!(f, " ∨ ")?;
                }
                first = false;

                if ands.vars.is_empty() && ands.negated_vars.is_empty() {
                    write!(f, "⊤")?;
                } else {
                    let mut first = true;

                    for var in &ands.vars {
                        if !first {
                            write!(f, " ∧ ")?;
                        }
                        first = false;

                        write!(f, "{var:?}")?;
                    }

                    for var in &ands.negated_vars {
                        if !first {
                            write!(f, " ∧ ")?;
                        }
                        first = false;

                        write!(f, "¬{var:?}")?;
                    }
                }
            }
        }

        Ok(())
    }
}
