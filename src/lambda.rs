use chumsky::{error::Simple, prelude::*, Parser};
use std::collections::HashSet;
use std::fmt;
use std::hash::Hash;

const ALPHABET: &str = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";

/// enum to allow writing variables as either x / x' / x''
#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
pub struct Variable {
    // the name of the variable
    c: char,
    // the number of prime symbols after the variable name
    primes: u32,
}

impl Variable {
    fn new(c: char) -> Self {
        Self { c, primes: 0 }
    }

    fn new_with_primes(c: char, primes: u32) -> Self {
        Self { c, primes }
    }

    fn parser() -> impl Parser<char, Variable, Error = Simple<char>> {
        one_of(ALPHABET)
            .then(just('\'').repeated())
            .map(|(c, v)| Variable {
                c,
                primes: v.len() as u32,
            })
    }
}

impl fmt::Display for Variable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}{}", self.c, "'".repeat(self.primes as usize))
    }
}

/// enum representing all of the lambda term cases
#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub enum Term {
    // bcVar: x
    Var(Variable),
    // scAbs: λx.t
    Abs(Variable, Box<Term>),
    // scApp: tt'
    App(Box<Term>, Box<Term>),
    // (t)
    // Brac(Box<Term>),
}

impl fmt::Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Term::Var(v) => write!(f, "{v}"),
            Term::Abs(v, t) => write!(f, "λ{v}.{t}"),
            Term::App(t1, t2) => write!(f, "{t1}{t2}"),
            // Term::Brac(t) => write!(f, "({t})"),
        }
    }
}

impl Term {
    #[allow(non_snake_case)]
    fn parser() -> impl Parser<char, Term, Error = Simple<char>> {
        recursive(|term| {
            // without left-recursion:
            //     TERM         -> APPLICATION | ABSTRACTION
            // ABSTRACTION  -> LAMBDA LCID DOT TERM
            // APPLICATION  -> ATOM APPLICATION'
            // APPLICATION' -> ATOM APPLICATION' | ε
            // ATOM         -> LPAREN TERM RPAREN | LCID
            // LCID         -> 'a' | 'b' | ... | 'z'
            // DOT          -> '.'
            // LAMBDA       -> 'λ'

            let bcVar = Variable::parser().map(Term::Var);

            let bracketed_term = term.clone().delimited_by(just('('), just(')'));

            let atom = bcVar.or(bracketed_term);

            let scApp = recursive(|app| {
                atom.then(app.or_not()).map(|(atm, arg)| match arg {
                    Some(arg) => Term::App(Box::new(atm), Box::new(arg)),
                    None => atm,
                })
            });

            let scAbs = just('λ')
                .ignore_then(Variable::parser())
                .then_ignore(just('.'))
                .then(term.clone())
                .map(|(v, t)| Term::Abs(v, Box::new(t)));

            scApp.or(scAbs)
        })
        .then_ignore(end())
    }
}

impl std::str::FromStr for Term {
    type Err = Vec<Simple<char>>;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Term::parser().parse(s)
    }
}

/// struct representing an entire expression, with sets of the terms and vars to allow rapid querying
#[derive(Debug, Clone)]
pub struct Expr {
    term: Term,
    // ^Trm
    terms: HashSet<Term>,
    // Vars
    vars: HashSet<Variable>,
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self)
    }
}

impl From<Term> for Expr {
    fn from(t: Term) -> Self {
        let mut stack: Vec<Term> = vec![t.clone()];
        let mut expr = Expr {
            term: t.clone(),
            terms: HashSet::from([t]),
            vars: HashSet::new(),
        };

        while let Some(term) = stack.pop() {
            match term {
                Term::Var(v) => {
                    expr.vars.insert(v);
                }
                Term::Abs(v, t) => {
                    expr.vars.insert(v);
                    expr.terms.insert(*t.clone());
                    stack.push(*t.clone());
                }
                Term::App(t1, t2) => {
                    expr.terms.insert(*t1.clone());
                    expr.terms.insert(*t2.clone());
                    stack.push(*t1.clone());
                    stack.push(*t2.clone());
                } // Term::Brac(t) => {
                  //     expr.terms.insert(*t.clone());
                  //     stack.push(*t.clone());
                  // }
            }
        }

        expr
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use Term::*;

    #[test]
    fn test_variable_parser() {
        for (var, prim) in [("x", 0), ("x'", 1), ("x''", 2)] {
            let v = Variable::parser().parse(var).unwrap();
            assert_eq!(
                v,
                Variable {
                    c: 'x',
                    primes: prim
                }
            )
        }
    }

    #[test]
    fn test_basic_var() {
        let s = "x";
        let correct = Var(Variable::new('x'));
        assert_eq!(s.parse::<Term>().unwrap(), correct);
    }

    #[test]
    fn test_basic_var_prime() {
        let s = "x'";
        let correct = Var(Variable::new_with_primes('x', 1));
        assert_eq!(s.parse::<Term>().unwrap(), correct);
    }

    #[test]
    fn test_basic_app() {
        let s = "xy";
        let x = Variable::new('x');
        let y = Variable::new('y');
        let correct = App(Box::new(Var(x)), Box::new(Var(y)));
        assert_eq!(s.parse::<Term>().unwrap(), correct);
    }

    #[test]
    fn test_basic_abs() {
        let s = "λx.y";
        let x = Variable::new('x');
        let y = Variable::new('y');
        let correct = Abs(x, Box::new(Var(y)));
        assert_eq!(s.parse::<Term>().unwrap(), correct);
    }

    // fn test_awful() {
    //     let s = "λw.w(λx.λy.λx.xbx)λx.x(λx.x)awy";
    //     let correct = todo!();
    // }
}
