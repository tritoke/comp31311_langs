use crate::lambda::Variable;
use crate::typed_lambda::{Type, TypeEnvironment};
use chumsky::{error::Simple, prelude::*, Parser};
use std::hash::Hash;
use std::str::FromStr;

/// enum representing a preterm - a term which has not yet been typechecked
#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub enum Preterm {
    /// Variable - bcPVar: x
    Var(Variable),
    /// Abstraction - scPAbs: λx:σ.t
    Abs(Variable, Type, Box<Preterm>),
    /// Application - scPApp: tt'
    App(Box<Preterm>, Box<Preterm>),
}

impl FromStr for Preterm {
    type Err = Vec<Simple<char>>;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Preterm::parser().parse(s)
    }
}

impl From<Variable> for Preterm {
    fn from(v: Variable) -> Self {
        Preterm::var(v)
    }
}

impl Preterm {
    /// Create a Preterm::Var from a variable
    /// ```
    /// use chumsky::Parser;
    /// use comp31311_langs::lambda::Variable;
    /// use comp31311_langs::typed_lambda::Preterm;
    /// let correct = Preterm::parser().parse(r"x").unwrap();
    /// let x = Variable::new('x');
    /// let v = Preterm::var(x);
    /// assert_eq!(v, correct);
    /// ```
    pub fn var(v: Variable) -> Self {
        Self::Var(v)
    }

    /// Create a Preterm::Abs from a variable and a term
    /// ```
    /// use chumsky::Parser;
    /// use comp31311_langs::lambda::Variable;
    /// use comp31311_langs::typed_lambda::{Preterm, Type};
    /// let correct = Preterm::parser().parse(r"\x:t.y").unwrap();
    /// let x = Variable::new('x');
    /// let y = Preterm::Var(Variable::new('y'));
    /// let t = Type::raw('t');
    /// let term = Preterm::abs(x, t, y);
    /// assert_eq!(term, correct);
    /// ```
    pub fn abs(v: Variable, ty: Type, t: Preterm) -> Self {
        Self::Abs(v, ty, Box::new(t))
    }

    /// Create a Preterm::App from two Preterms
    /// ```
    /// use chumsky::Parser;
    /// use comp31311_langs::lambda::Variable;
    /// use comp31311_langs::typed_lambda::Preterm;
    /// let correct = Preterm::parser().parse(r"xy").unwrap();
    /// let x = Preterm::Var(Variable::new('x'));
    /// let y = Preterm::Var(Variable::new('y'));
    /// let term = Preterm::app(x, y);
    /// assert_eq!(term, correct);
    /// ```
    pub fn app(t1: Preterm, t2: Preterm) -> Self {
        Self::App(Box::new(t1), Box::new(t2))
    }

    /// the chumsky parser for the Preterm type
    /// ```
    /// use chumsky::Parser;
    /// use comp31311_langs::lambda::Variable;
    /// use comp31311_langs::typed_lambda::{Preterm, Type};
    /// let t = Preterm::parser().parse(r"\c:t.abc").unwrap();
    /// let [a, b, c] = ['a', 'b', 'c'].map(Variable::new);
    /// let ty = Type::raw('t');
    /// let correct = Preterm::abs(c, ty, Preterm::app(Preterm::app(a.into(), b.into()), c.into()));
    /// assert_eq!(t, correct);
    /// ```
    pub fn parser() -> impl Parser<char, Self, Error = Simple<char>> {
        recursive(|term| {
            let bc_var = Variable::parser().map(Preterm::Var);
            let bracketed_term = choice((
                term.clone().delimited_by(just('('), just(')')),
                term.clone().delimited_by(just('['), just(']')),
                term.clone().delimited_by(just('<'), just('>')),
            ));
            let atom = bc_var.or(bracketed_term);
            let sc_abs = one_of(r"λ\")
                .ignore_then(Variable::parser())
                .then_ignore(just(':'))
                .then(Type::parser())
                .then_ignore(just('.'))
                .then(term.clone())
                .map(|((v, ty), t): ((Variable, Type), Preterm)| Preterm::abs(v, ty, t));
            let sc_app = atom.clone().then(atom.repeated()).foldl(Preterm::app);

            sc_app.or(sc_abs)
        })
        .then_ignore(end())
    }

    /// Does the Preterm have the type ty, given a list of assumptions - gamma
    pub fn has_type(&self, mut gamma: TypeEnvironment, ty: Type) -> bool {
        match self {
            // bcVar
            Preterm::Var(v) => gamma.current_assumption(v) == Some(ty),
            // scAbs
            Preterm::Abs(v, ty, _t) => {
                gamma.assume(*v, ty.clone());
                todo!()
            }
            // scApp
            Preterm::App(_t1, _t2) => {
                todo!()
            }
        }
    }
}
