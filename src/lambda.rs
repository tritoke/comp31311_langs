use chumsky::{error::Simple, prelude::*, Parser};
use std::collections::HashSet;
use std::fmt;
use std::fmt::Formatter;
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

#[allow(unused)]
impl Variable {
    fn new(c: char) -> Self {
        Self { c, primes: 0 }
    }

    fn new_with_primes(c: char, primes: u32) -> Self {
        Self { c, primes }
    }

    fn parser() -> impl Parser<char, Variable, Error = Simple<char>> + Clone {
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
}

// struct to hold the left hand side of an application while formatting
struct Left(Term);

impl fmt::Display for Left {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match &self.0 {
            Term::Var(v) => write!(f, "{v}"),
            Term::Abs(v, t) => write!(f, "λ{v}.{t}"),
            Term::App(t1, t2) => {
                let l1 = Left(*t1.clone());
                let l2 = Left(*t2.clone());
                match (t1.is_abs(), t2.is_abs()) {
                    (true, true) => write!(f, "({l1})({l2})"),
                    (true, false) => write!(f, "({l1}){l2}"),
                    (false, true) => write!(f, "{l1}({l2})"),
                    (false, false) => write!(f, "{l1}{l2}"),
                }
            }
        }
    }
}

impl fmt::Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if f.alternate() {
            // in the alternate representation fully bracket the expression
            match self {
                Term::Var(v) => write!(f, "{v}"),
                Term::Abs(v, t) => write!(f, "(λ{v}.{t:#})"),
                Term::App(t1, t2) => write!(f, "({t1:#}{t2:#})"),
            }
        } else {
            match self {
                Term::Var(v) => write!(f, "{v}"),
                Term::Abs(v, t) => write!(f, "λ{v}.{t}"),
                Term::App(t1, t2) => write!(f, "{}{t2}", Left(*t1.clone())),
            }
        }
    }
}

#[allow(unused)]
impl Term {
    fn var(v: Variable) -> Self {
        Self::Var(v)
    }

    fn abs(v: Variable, t: Term) -> Self {
        Self::Abs(v, Box::new(t))
    }

    fn app(t1: Term, t2: Term) -> Self {
        Self::App(Box::new(t1), Box::new(t2))
    }

    fn is_var(&self) -> bool {
        matches!(self, Term::Var(_))
    }

    fn is_abs(&self) -> bool {
        matches!(self, Term::Abs(_, _))
    }

    fn is_app(&self) -> bool {
        matches!(self, Term::App(_, _))
    }

    fn parser() -> impl Parser<char, Term, Error = Simple<char>> {
        recursive(|term| {
            // without left-recursion:
            //     TERM         -> APPLICATION | ABSTRACTION
            // ABSTRACTION  -> LAMBDA LCID DOT TERM
            // APPLICATION  -> ATOM APPLICATION'
            // APPLICATION' -> ATOM | ABSTRACTION | APPLICATION' | ε
            // ATOM         -> LPAREN TERM RPAREN | LCID
            // LCID         -> 'a' | 'b' | ... | 'z'
            // DOT          -> '.'
            // LAMBDA       -> 'λ'

            let bc_var = Variable::parser().map(Term::Var);
            let bracketed_term = term.clone().delimited_by(just('('), just(')'));
            let atom = bc_var.or(bracketed_term);
            let sc_abs = just('λ')
                .ignore_then(Variable::parser())
                .then_ignore(just('.'))
                .then(term.clone())
                .map(|(v, t)| Term::abs(v, t));
            let sc_app = atom
                .clone()
                .then(atom.or(sc_abs.clone()).clone().repeated())
                .foldl(Term::app);

            sc_app.or(sc_abs)
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
                }
            }
        }

        expr
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use Term::*;

    macro_rules! vars {
        ($name:ident) => {
            let $name: Variable = Variable::parser().parse(stringify!($name)).unwrap();
        };
        ($($name:ident)*) => {
            let (
                $($name),*
            ) = (
                $(Variable::parser().parse(stringify!($name)).unwrap()),*
            );
        };
    }

    macro_rules! term {
        (var $v:expr) => {
            Term::Var($v)
        };
        (vars $($v:expr)*) => {
            (
                $( Term::Var($v) ),*
            )
        };
        (app $($ts:expr)*) => {{
            let v = Vec::from([
                $($ts),*
            ]);
            v.into_iter().reduce(|a, b| Term::App(Box::new(a), Box::new(b))).unwrap()
        }};
        (abs $v:expr => $t:expr) => {
            Term::Abs($v, Box::new($t))
        };
    }

    /// macro to perform both the parsing and formatting tests
    macro_rules! do_test {
        ($s:ident, $correct:ident) => {{
            let parsed = $s.parse::<Term>().unwrap();
            assert_eq!(parsed, $correct);
            assert_eq!(format!("{}", parsed), $s);
        }};
    }

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
        do_test!(s, correct);
    }

    #[test]
    fn test_basic_var_prime() {
        let s = "x'";
        let correct = Var(Variable::new_with_primes('x', 1));
        do_test!(s, correct);
    }

    #[test]
    fn test_basic_app() {
        let s = "xy";
        vars!(x y);
        let correct = App(Box::new(Var(x)), Box::new(Var(y)));
        do_test!(s, correct);
    }

    #[test]
    fn test_basic_abs() {
        let s = "λx.y";
        vars!(x y);
        let correct = Abs(x, Box::new(Var(y)));
        do_test!(s, correct);
    }

    #[test]
    fn test_app_chain_short() {
        let s = "abc";
        vars!(a b c);
        let (a, b, c) = term!(vars a b c);
        let correct = term!(app a b c);
        do_test!(s, correct);
    }

    #[test]
    fn test_app_chain() {
        let s = "abcdef";
        vars!(a b c d e f);
        let (a, b, c, d, e, f) = term!(vars a b c d e f);
        let correct = term!(app a b c d e f);
        do_test!(s, correct);
    }

    #[test]
    fn test_abs_chain() {
        let s = "λx.λy.λz.xyz";
        vars!(x y z);
        let (xt, yt, zt) = term!(vars x y z);
        let app = term!(app xt yt zt);
        let tmp1 = term!(abs z => app);
        let tmp2 = term!(abs y => tmp1);
        let correct = term!(abs x => tmp2);
        do_test!(s, correct);
    }

    #[test]
    fn test_abs_after_var() {
        let s = "xλx.x";
        vars!(x);
        let xt = term!(var x);
        let abs = term!(abs x => xt.clone());
        let correct = term!(app xt abs);
        do_test!(s, correct);
    }

    #[test]
    fn test_worksheet1() {
        let s = "λw.w(λx.λy.λx.xbx)λx.x(λx.x)awy";

        vars!(a b w x y);

        let (at, bt, wt, xt, yt) = term!(vars a b w x y);
        let tmp = term!(app xt.clone() bt xt.clone()); // xbx
        let tmp = term!(abs x => tmp); // λx.xbx
        let tmp = term!(abs y => tmp); // λy.λx.xbx
        let left_brac = term!(abs x => tmp); // λx.λy.λx.xbx
        let right_brac = term!(abs x => xt.clone()); // λx.x
        let right_abs_inner = term!(app xt right_brac at wt.clone() yt); // x(λx.x)awy
        let right_abs = term!(abs x => right_abs_inner); // λx.x(λx.x)awy
        let abs_inner = term!(app wt left_brac right_abs); // w(λx.λy.λx.xbx)λx.x(λx.x)awy
        let correct = term!(abs w => abs_inner); // λw.w(λx.λy.λx.xbx)λx.x(λx.x)awy

        do_test!(s, correct);
    }

    #[test]
    fn test_nested_bracketing() {
        let s = "(λx.x)(λy.y)λz.z";
        vars!(x y z);
        let (xt, yt, zt) = term!(vars x y z);
        let x_brac = term!(abs x => xt); // λx.x
        let y_brac = term!(abs y => yt); // λx.x
        let dangle_dangle = term!(abs z => zt); // λx.x
        let correct = term!(app x_brac y_brac dangle_dangle);
        do_test!(s, correct);
    }
}
