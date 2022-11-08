use crate::lambda::Variable;
use crate::typed_lambda::{Preterm, Type};
use chumsky::{error::Simple, prelude::*, Parser};
use std::collections::HashMap;
use std::fmt;
use std::fmt::Write;

/// Struct for representing a list of currently held type assumptions
#[derive(Default, Debug, Clone, Eq, PartialEq)]
pub struct TypeEnvironment {
    assumptions: HashMap<Variable, Vec<Type>>,
}

impl TypeEnvironment {
    /// Create an empty list of type assumptions
    /// ```
    /// use comp31311_langs::typed_lambda::TypeEnvironment;
    /// let ass = TypeEnvironment::new();
    /// ```
    pub fn new() -> Self {
        Default::default()
    }

    /// Add an assumption to the list of assumptions for a type
    /// ```
    /// use comp31311_langs::lambda::Variable;
    /// use comp31311_langs::typed_lambda::{Type, TypeEnvironment};
    /// let mut ass = TypeEnvironment::new();
    /// let sigma = Type::raw('σ');
    /// let x = Variable::new('x');
    /// ass.assume(x, sigma);
    /// ```
    pub fn assume(&mut self, var: Variable, ty: Type) {
        self.assumptions.entry(var).or_default().push(ty)
    }

    /// Pop off the top level assumption for a type
    /// ```
    /// use comp31311_langs::lambda::Variable;
    /// use comp31311_langs::typed_lambda::{Type, TypeEnvironment};
    /// let mut ass = TypeEnvironment::new();
    /// let [sigma, tau] = ['σ', 'τ'].map(Type::raw);
    /// let x = Variable::new('x');
    /// ass.assume(x, sigma.clone());
    /// ass.assume(x, tau.clone());
    /// assert_eq!(ass.pop_assumption(&x), Some(tau));
    /// assert_eq!(ass.pop_assumption(&x), Some(sigma));
    /// assert_eq!(ass.pop_assumption(&x), None);
    /// ```
    pub fn pop_assumption(&mut self, var: &Variable) -> Option<Type> {
        self.assumptions.get_mut(var).and_then(|ass| ass.pop())
    }

    /// Get the current type assumption for a given type
    /// ```
    /// use comp31311_langs::lambda::Variable;
    /// use comp31311_langs::typed_lambda::{Type, TypeEnvironment};
    /// let mut ass = TypeEnvironment::new();
    /// let [sigma, tau] = ['σ', 'τ'].map(Type::raw);
    /// let x = Variable::new('x');
    /// ass.assume(x, sigma.clone());
    /// ass.assume(x, tau.clone());
    /// assert_eq!(ass.current_assumption(&x), Some(tau));
    /// ```
    pub fn current_assumption(&self, var: &Variable) -> Option<Type> {
        self.assumptions.get(var).and_then(|v| v.last().cloned())
    }

    /// The chumsky parser for the TypeEnvironment type
    /// ```
    /// use chumsky::Parser;
    /// use comp31311_langs::typed_lambda::{TypeEnvironment, Type};
    /// use comp31311_langs::lambda::Variable;
    /// let te: TypeEnvironment = "a:b, c:d, c:d->d".parse().unwrap();
    /// let [a, c] = ['a', 'c'].map(Variable::new);
    /// let [b, d] = ['b', 'd'].map(Type::raw);
    /// let mut correct = TypeEnvironment::new();
    /// correct.assume(a, b);
    /// correct.assume(c, d.clone());
    /// correct.assume(c, Type::function(d.clone(), d.clone()));
    /// assert_eq!(te, correct);
    /// ```
    pub fn parser() -> impl Parser<char, TypeEnvironment, Error = Simple<char>> {
        Variable::parser()
            .then_ignore(just(':'))
            .then(Type::parser())
            .padded()
            .separated_by(just(','))
            .map(|assumptions| assumptions.into_iter().collect())
    }

    /// Attempt to infer the type of the Preterm
    /// inferring a type == determining a type from an empty type environment
    /// ```
    /// use comp31311_langs::typed_lambda::{TypeEnvironment, Preterm, Type};
    /// use comp31311_langs::lambda::Variable;
    ///
    /// let [f, x] = ['f', 'x'].map(Variable::new);
    /// let a = Type::raw('a');
    /// let pt: Preterm = r"\f:a->a.\x:a.fx".parse().unwrap();
    /// let correct: Type = "(a->a)->a->a".parse().unwrap();
    /// let inferred = TypeEnvironment::new().infer_type(&pt);
    /// assert_eq!(inferred, Some(correct));
    /// ```
    pub fn infer_type(&mut self, pt: &Preterm) -> Option<Type> {
        let out = match pt {
            // t:τ
            Preterm::Var(v) => self.current_assumption(v),
            // \x:σ.t:σ->τ, t:τ
            Preterm::Abs(v, ty, t) => {
                self.assume(*v, ty.clone());
                let inferred = self.infer_type(t);
                self.pop_assumption(v);
                Some(Type::function(ty.clone(), inferred?))
            }
            // f:σ->τ, t:σ, ft:τ
            Preterm::App(s, t) => {
                let st = self.infer_type(s)?;
                let tt = self.infer_type(t);
                match st {
                    Type::Function(_, target) => match tt {
                        Some(x) if &x == &*target => Some(*target),
                        None => Some(*target),
                        _ => None,
                    },
                    _ => None,
                }
            }
        };
        eprint!("[infer_type] self={self}, pt={pt}");
        if let Some(ref o) = out {
            eprintln!(", out=Some({o})");
        } else {
            eprintln!(", out=None");
        }
        out
    }
}

impl<const N: usize> From<[(Variable, Type); N]> for TypeEnvironment {
    fn from(assumptions: [(Variable, Type); N]) -> Self {
        let mut te = TypeEnvironment::new();
        for (var, ty) in assumptions {
            te.assume(var, ty);
        }
        te
    }
}

impl FromIterator<(Variable, Type)> for TypeEnvironment {
    fn from_iter<T: IntoIterator<Item = (Variable, Type)>>(iter: T) -> Self {
        let mut te = TypeEnvironment::new();
        for (var, ty) in iter.into_iter() {
            te.assume(var, ty);
        }
        te
    }
}

impl std::str::FromStr for TypeEnvironment {
    type Err = Vec<Simple<char>>;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        TypeEnvironment::parser().parse(s)
    }
}

impl fmt::Display for TypeEnvironment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut first = true;

        for (v, t) in self
            .assumptions
            .iter()
            .flat_map(|(v, tys)| std::iter::repeat(v).zip(tys.iter()))
        {
            if first {
                first = false;
            } else {
                f.write_char(',')?;
            }
            write!(f, "{v}:{t}")?;
        }

        Ok(())
    }
}
