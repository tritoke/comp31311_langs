use crate::lambda::Variable;
use chumsky::{error::Simple, prelude::*, Parser};
use std::collections::HashSet;
use std::hash::Hash;
use std::str::FromStr;

/// enum representing all of the lambda term cases
#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub enum Term {
    /// Variable - bcVar: x
    Var(Variable),
    /// Abstraction - scAbs: λx.t
    Abs(Variable, Box<Term>),
    /// Application - scApp: tt'
    App(Box<Term>, Box<Term>),
}

/// Type alias representing a set of Variables
pub type Vars = HashSet<Variable>;

impl FromStr for Term {
    type Err = Vec<Simple<char>>;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Term::parser().parse(s)
    }
}

impl From<Variable> for Term {
    fn from(v: Variable) -> Self {
        Term::Var(v)
    }
}

impl Term {
    /// Create a Term::Var from a variable
    /// ```
    /// use chumsky::Parser;
    /// use lambda_parser::lambda::{Term, Variable};
    /// let correct = Term::parser().parse(r"x").unwrap();
    /// let x = Variable::new('x');
    /// let v = Term::var(x);
    /// assert_eq!(v, correct);
    /// ```
    pub fn var(v: Variable) -> Self {
        Self::Var(v)
    }

    /// Create a Term::Abs from a variable and a term
    /// ```
    /// use chumsky::Parser;
    /// use lambda_parser::lambda::{Term, Variable};
    /// let correct = Term::parser().parse(r"\x.y").unwrap();
    /// let x = Variable::new('x');
    /// let y = Term::Var(Variable::new('y'));
    /// let term = Term::abs(x, y);
    /// assert_eq!(term, correct);
    /// ```
    pub fn abs(v: Variable, t: Term) -> Self {
        Self::Abs(v, Box::new(t))
    }

    /// Create a Term::App from two Terms
    /// ```
    /// use chumsky::Parser;
    /// use lambda_parser::lambda::{Term, Variable};
    /// let correct = Term::parser().parse(r"xy").unwrap();
    /// let x = Term::Var(Variable::new('x'));
    /// let y = Term::Var(Variable::new('y'));
    /// let term = Term::app(x, y);
    /// assert_eq!(term, correct);
    /// ```
    pub fn app(t1: Term, t2: Term) -> Self {
        Self::App(Box::new(t1), Box::new(t2))
    }

    /// Is the Term a Term::Var?
    /// ```
    /// use chumsky::Parser;
    /// use lambda_parser::lambda::Term;
    /// let term = Term::parser().parse(r"x").unwrap();
    /// assert!(term.is_var());
    /// ```
    pub fn is_var(&self) -> bool {
        matches!(self, Term::Var(_))
    }

    /// Is the Term a Term::Abs?
    /// ```
    /// use chumsky::Parser;
    /// use lambda_parser::lambda::Term;
    /// let term = Term::parser().parse(r"\x.y").unwrap();
    /// assert!(term.is_abs());
    /// ```
    pub fn is_abs(&self) -> bool {
        matches!(self, Term::Abs(_, _))
    }

    /// Is the Term a Term::App?
    /// ```
    /// use chumsky::Parser;
    /// use lambda_parser::lambda::Term;
    /// let term = Term::parser().parse(r"xy").unwrap();
    /// assert!(term.is_app());
    /// ```
    pub fn is_app(&self) -> bool {
        matches!(self, Term::App(_, _))
    }

    /// the chumsky parser for the Term type
    /// ```
    /// use chumsky::Parser;
    /// use lambda_parser::lambda::{Term, Variable};
    /// let t = Term::parser().parse(r"\c.abc").unwrap();
    /// let a = Variable::new('a');
    /// let b = Variable::new('b');
    /// let c = Variable::new('c');
    /// let correct = Term::abs(c, Term::app(Term::app(a.into(), b.into()), c.into()));
    /// assert_eq!(t, correct);
    /// ```
    pub fn parser() -> impl Parser<char, Term, Error = Simple<char>> {
        recursive(|term| {
            // without left-recursion:
            //     TERM         -> APPLICATION | ABSTRACTION
            // ABSTRACTION  -> LAMBDA LCID DOT TERM
            // APPLICATION  -> ATOM APPLICATION'
            // APPLICATION' -> ATOM | ABSTRACTION | APPLICATION' | ε
            // ATOM         -> LPAREN TERM RPAREN | LCID
            // LCID         -> 'a' | 'b' | ... | 'z'
            // DOT          -> '.'
            // LAMBDA       -> 'λ' | '\'

            let bc_var = Variable::parser().map(Term::Var);
            let bracketed_term = term.clone().delimited_by(just('('), just(')'));
            let atom = bc_var.or(bracketed_term);
            let sc_abs = one_of(r"λ\")
                .ignore_then(Variable::parser())
                .then_ignore(just('.'))
                .then(term.clone())
                .map(|(v, t): (Variable, Term)| Term::abs(v, t));
            let sc_app = atom
                .clone()
                .then(atom.or(sc_abs.clone()).clone().repeated())
                .foldl(Term::app);

            sc_app.or(sc_abs)
        })
        .then_ignore(end())
    }

    /// Is the term `t` a subterm of the given term
    /// ```
    /// use chumsky::Parser;
    /// use lambda_parser::lambda::Term;
    /// let app = Term::parser().parse(r"\x.y").unwrap();
    /// let nested = Term::parser().parse(r"\a.\b.\c.\x.y").unwrap();
    /// assert!(app.is_subterm(&app));
    /// assert!(nested.is_subterm(&app));
    /// ```
    pub fn is_subterm(&self, t: &Term) -> bool {
        // first check if the self and t are identical
        if self == t {
            return true;
        }

        match self {
            // bcVar
            Term::Var(_) => self == t,
            // scAbs
            Term::Abs(v, t_inner) => t == &Term::Var(*v) || t_inner.is_subterm(t),
            // scApp
            Term::App(t1, t2) => t1.is_subterm(t) || t2.is_subterm(t),
        }
    }

    /// Is the term `t` a proper subterm of the given term
    /// ```
    /// use chumsky::Parser;
    /// use lambda_parser::lambda::Term;
    /// let app = Term::parser().parse(r"\x.y").unwrap();
    /// let nested = Term::parser().parse(r"\a.\b.\c.\x.y").unwrap();
    /// assert!(!app.is_proper_subterm(&app));
    /// assert!(nested.is_proper_subterm(&app));
    /// ```
    pub fn is_proper_subterm(&self, t: &Term) -> bool {
        if self == t {
            false
        } else {
            self.is_subterm(t)
        }
    }

    /// Return the set of all bound variables in this expression
    /// ```
    /// use chumsky::Parser;
    /// use lambda_parser::lambda::{Term, Variable, Vars};
    /// use std::collections::HashSet;
    /// let t = Term::parser().parse(r"\a.\b.\c.def").unwrap();
    /// let s: Vars = "abc".chars().map(Variable::new).collect();
    /// assert_eq!(t.bv(), s);
    /// ```
    pub fn bv(&self) -> Vars {
        match self {
            // bcVarbv
            Term::Var(_) => HashSet::new(),
            // scAbsbv
            Term::Abs(v, t) => t.bv().union(&HashSet::from([*v])).copied().collect(),
            // scAppbv
            Term::App(t1, t2) => t1.bv().union(&t2.bv()).copied().collect(),
        }
    }

    /// Return the set of all free variables in this expression
    /// ```
    /// use chumsky::Parser;
    /// use lambda_parser::lambda::{Term, Variable, Vars};
    /// use std::collections::HashSet;
    /// let t = Term::parser().parse(r"\a.\b.\c.def").unwrap();
    /// let s: Vars = "def".chars().map(Variable::new).collect();
    /// assert_eq!(t.fv(), s);
    /// ```
    pub fn fv(&self) -> Vars {
        match self {
            // bcVarfv
            Term::Var(v) => HashSet::from([*v]),
            // scAbsfv
            Term::Abs(v, t) => t.fv().difference(&HashSet::from([*v])).copied().collect(),
            // scAppfv
            Term::App(t1, t2) => t1.fv().union(&t2.fv()).copied().collect(),
        }
    }

    /// Return all the variables in a given Term
    /// ```
    /// use chumsky::Parser;
    /// use lambda_parser::lambda::{Term, Variable, Vars};
    /// use std::collections::HashSet;
    /// let t = Term::parser().parse(r"\a.\b.\c.def").unwrap();
    /// let s: Vars = "abcdef".chars().map(Variable::new).collect();
    /// assert_eq!(t.vars(), s);
    /// ```
    pub fn vars(&self) -> Vars {
        self.fv().union(&self.bv()).copied().collect()
    }

    /// Renaming variable A to variable B in term
    /// ```
    /// use chumsky::Parser;
    /// use lambda_parser::lambda::{Term, Variable};
    /// let t = Term::parser().parse(r"\a.\b.\c.abc").unwrap();
    /// let a = Variable::new('a');
    /// let b = Variable::new('b');
    /// let correct = Term::parser().parse(r"\b.\b.\c.bbc").unwrap();
    /// assert_eq!(t.ren(a, b), correct);
    /// ```
    pub fn ren(&self, a: Variable, b: Variable) -> Term {
        match self {
            // bcVarren
            Term::Var(x) => Term::Var(x.ren(a, b)),
            // scAbsren
            Term::Abs(x, t) => Term::abs(x.ren(a, b), t.ren(a, b)),
            // scAppren
            Term::App(t1, t2) => Term::app(t1.ren(a, b), t2.ren(a, b)),
        }
    }

    /// is this term alpha equivalent to `other`
    /// ```
    /// use chumsky::Parser;
    /// use lambda_parser::lambda::{Term, Variable};
    /// let t1 = Term::parser().parse(r"\a.\b.\c.abc").unwrap();
    /// let t2 = Term::parser().parse(r"\x.\y.\z.xyz").unwrap();
    /// assert!(t1.is_alpha_equivalent(&t2));
    /// ```
    pub fn is_alpha_equivalent(&self, other: &Term) -> bool {
        match (self, other) {
            // bcVar~α
            (Term::Var(v1), Term::Var(v2)) => v1 == v2,
            // scAbs~α
            (Term::Abs(x1, t1), Term::Abs(x2, t2)) => {
                let vs = self.vars().union(&other.vars()).copied().collect();
                let v = freshv(&vs);
                let r1 = t1.ren(*x1, v);
                let r2 = t2.ren(*x2, v);
                r1.is_alpha_equivalent(&r2)
            }
            // scApp~α
            (Term::App(t1, u1), Term::App(t2, u2)) => {
                t1.is_alpha_equivalent(t2) && u1.is_alpha_equivalent(u2)
            }
            // anything else is not alpha equivalent
            (_, _) => false,
        }
    }

    /// Perform capture avoiding substitution of the variable x for the term t in the current term
    /// ```
    /// use chumsky::Parser;
    /// use lambda_parser::lambda::{Term, Variable};
    /// let orig = Term::parser().parse(r"(\x.y)(\y.y)").unwrap();
    /// let a = Variable::new('y');
    /// let t = Term::parser().parse(r"\a.a").unwrap();
    /// let correct = Term::parser().parse(r"(\x.(\a.a))(\y.y)").unwrap();
    /// assert!(orig.cap_avoid_subst(a, t).is_alpha_equivalent(&correct));
    /// ```
    pub fn cap_avoid_subst(&self, x: Variable, t: Term) -> Term {
        match self {
            // bcVar[]
            Term::Var(y) => {
                if *y == x {
                    t
                } else {
                    self.clone()
                }
            }
            // scAbs[]
            Term::Abs(y, u) => {
                let mut vs: Vars = self.vars().union(&t.vars()).copied().collect();
                vs.insert(x);
                let w = freshv(&vs);
                Term::abs(w, u.ren(*y, w).cap_avoid_subst(x, t))
            }
            // scApp[]
            Term::App(r, s) => Term::app(r.cap_avoid_subst(x, t.clone()), s.cap_avoid_subst(x, t)),
        }
    }

    /// Perform one step of beta reduction if possible
    /// ```
    /// use chumsky::Parser;
    /// use lambda_parser::lambda::{Term, Variable};
    /// let orig = Term::parser().parse(r"(\x.x)(\y.y)").unwrap();
    /// let correct = Term::parser().parse(r"\y.y").unwrap();
    /// assert_eq!(orig.beta_reduction(), Some(correct));
    /// ```
    pub fn beta_reduction(&self) -> Option<Term> {
        match self {
            // bcVarβ
            Term::App(abs, a) if abs.is_abs() => match *abs.clone() {
                Term::Abs(x, t) => Some(t.cap_avoid_subst(x, *a.clone())),
                _ => None,
            },
            // scAbsβ
            Term::Abs(x, t) => t.beta_reduction().map(|tp| Term::abs(*x, tp)),
            // scAppβ
            Term::App(t, u) => t
                .beta_reduction()
                .map(|tp| Term::app(tp, *u.clone()))
                .or_else(|| u.beta_reduction().map(|up| Term::app(*t.clone(), up))),
            // if its none of these cases it doesn't beta reduce
            _ => None,
        }
    }
}

/// Generate a fresh variable given the variables in the term
pub fn freshv(vars: &Vars) -> Variable {
    // find the smallest letter in the alphabet with the
    // fewest number of primes to use as a fresh variable
    for primes in 0..=u32::MAX {
        for c in Variable::ALPHABET.chars() {
            let v = Variable::new_with_primes(c, primes);
            if !vars.contains(&v) {
                return v;
            }
        }
    }

    unreachable!("Big bruh moment - ran out of variables?????????");
}
