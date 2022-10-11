use crate::lambda::Term::Var;
use chumsky::{error::Simple, prelude::*, Parser};
use std::fmt;
use std::hash::Hash;

/// enum to allow writing variables as either x / x' / x''
#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub struct Variable {
    // the name of the variable
    c: char,
    // the number of prime symbols after the variable name
    primes: u32,
}

impl From<char> for Variable {
    fn from(c: char) -> Self {
        Variable::new(c)
    }
}

#[allow(unused)]
impl Variable {
    /// The alphabet of characters allowed to be a variable name
    pub const ALPHABET: &'static str = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";

    /// Create a new variable with just an alphabetic character
    pub fn new(c: char) -> Self {
        assert!(c.is_alphabetic());
        Self { c, primes: 0 }
    }

    /// Create a new variable with an alphabetic character and a number of prime symbols
    pub fn new_with_primes(c: char, primes: u32) -> Self {
        assert!(c.is_alphabetic());
        Self { c, primes }
    }

    /// Chumsky parser for the Variable type
    pub fn parser() -> impl Parser<char, Variable, Error = Simple<char>> + Clone {
        one_of(Variable::ALPHABET)
            .then(just('\'').repeated())
            .map(|(c, v)| Variable {
                c,
                primes: v.len() as u32,
            })
    }

    /// Rename the variable if it matches
    pub(super) fn ren(self, a: Variable, b: Variable) -> Variable {
        if self == a {
            b
        } else {
            self
        }
    }
}

impl fmt::Display for Variable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}{}", self.c, "'".repeat(self.primes as usize))
    }
}
