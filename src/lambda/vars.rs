use crate::lambda::Variable;
use std::collections::HashSet;
use std::fmt;
use std::fmt::Write;

/// Type alias representing a set of Variables
#[derive(Default, Debug, Clone, Eq, PartialEq)]
pub struct Vars {
    vars: HashSet<Variable>,
}

impl Vars {
    /// construct a new empty set of variables
    /// ```
    /// use comp31311_langs::lambda::Vars;
    /// let vars = Vars::new();
    /// ```
    pub fn new() -> Self {
        Default::default()
    }

    /// construct the union of two sets of variables
    /// ```
    /// use comp31311_langs::lambda::{Vars, Variable};
    /// let a: Vars = "abc".chars().map(Variable::new).collect();
    /// let b: Vars = "cde".chars().map(Variable::new).collect();
    /// let union: Vars = "abcde".chars().map(Variable::new).collect();
    /// assert_eq!(a.union(b), union);
    /// ```
    pub fn union(&self, other: impl Into<Self>) -> Self {
        self.vars.union(&other.into().vars).copied().collect()
    }

    /// construct the difference of two sets of variables
    /// ```
    /// use comp31311_langs::lambda::{Vars, Variable};
    /// let a: Vars = "abc".chars().map(Variable::new).collect();
    /// let b: Vars = "cde".chars().map(Variable::new).collect();
    /// let diff: Vars = "ab".chars().map(Variable::new).collect();
    /// assert_eq!(a.difference(b), diff);
    /// ```
    pub fn difference(&self, other: impl Into<Self>) -> Self {
        self.vars.difference(&other.into().vars).copied().collect()
    }

    /// Generate a fresh variable given the variables in the term
    /// ```
    /// use comp31311_langs::lambda::{Vars, Variable};
    /// let vs: Vars = "abc".chars().map(Variable::new).collect();
    /// assert_eq!(vs.freshv(), Variable::new('d'));
    /// ```
    pub fn freshv(&self) -> Variable {
        // find the smallest letter in the alphabet with the
        // fewest number of primes to use as a fresh variable
        for primes in 0..=u32::MAX {
            for c in Variable::ALPHABET.chars() {
                let v = Variable::new_with_primes(c, primes);
                if !self.vars.contains(&v) {
                    return v;
                }
            }
        }

        unreachable!("Big bruh moment - ran out of variables?????????");
    }
}

impl From<HashSet<Variable>> for Vars {
    fn from(vars: HashSet<Variable>) -> Self {
        Vars { vars }
    }
}

impl<const N: usize> From<[Variable; N]> for Vars {
    fn from(vars: [Variable; N]) -> Self {
        Self {
            vars: vars.into_iter().collect(),
        }
    }
}

impl FromIterator<Variable> for Vars {
    fn from_iter<T: IntoIterator<Item = Variable>>(iter: T) -> Self {
        Self {
            vars: iter.into_iter().collect(),
        }
    }
}

impl fmt::Display for Vars {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_char('{')?;

        let mut first = true;
        for v in &self.vars {
            if first {
                first = false;
                write!(f, "{v}")?;
            } else {
                write!(f, ", {v}")?;
            }
        }

        f.write_char('}')?;

        Ok(())
    }
}
