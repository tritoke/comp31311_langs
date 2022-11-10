use crate::lambda::{Variable, Vars};
use chumsky::{error::Simple, prelude::*, Parser};
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
    /// use comp31311_langs::lambda::{Term, Variable};
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
    /// use comp31311_langs::lambda::{Term, Variable};
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
    /// use comp31311_langs::lambda::{Term, Variable};
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
    /// use comp31311_langs::lambda::Term;
    /// let term = Term::parser().parse(r"x").unwrap();
    /// assert!(term.is_var());
    /// ```
    pub fn is_var(&self) -> bool {
        matches!(self, Term::Var(_))
    }

    /// Is the Term a Term::Abs?
    /// ```
    /// use chumsky::Parser;
    /// use comp31311_langs::lambda::Term;
    /// let term = Term::parser().parse(r"\x.y").unwrap();
    /// assert!(term.is_abs());
    /// ```
    pub fn is_abs(&self) -> bool {
        matches!(self, Term::Abs(_, _))
    }

    /// Is the Term a Term::App?
    /// ```
    /// use chumsky::Parser;
    /// use comp31311_langs::lambda::Term;
    /// let term = Term::parser().parse(r"xy").unwrap();
    /// assert!(term.is_app());
    /// ```
    pub fn is_app(&self) -> bool {
        matches!(self, Term::App(_, _))
    }

    /// the chumsky parser for the Term type
    /// ```
    /// use chumsky::Parser;
    /// use comp31311_langs::lambda::{Term, Variable};
    /// let t = Term::parser().parse(r"\c.abc").unwrap();
    /// let [a, b, c] = ['a', 'b', 'c'].map(Variable::new);
    /// let correct = Term::abs(c, Term::app(Term::app(a.into(), b.into()), c.into()));
    /// assert_eq!(t, correct);
    /// ```
    pub fn parser() -> impl Parser<char, Term, Error = Simple<char>> {
        recursive(|term| {
            let bc_var = Variable::parser().map(Term::Var);
            let bracketed_term = choice((
                term.clone().delimited_by(just('('), just(')')),
                term.clone().delimited_by(just('['), just(']')),
                term.clone().delimited_by(just('<'), just('>')),
            ));
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
    /// use comp31311_langs::lambda::Term;
    /// let app = Term::parser().parse(r"\x.y").unwrap();
    /// let nested = Term::parser().parse(r"\a.\b.\c.\x.y").unwrap();
    /// assert!(app.is_subterm(&app));
    /// assert!(app.is_subterm(&nested));
    /// ```
    pub fn is_subterm(&self, t: &Term) -> bool {
        // first check if the self and t are identical
        if self == t {
            return true;
        }

        match t {
            // bcVar
            Term::Var(_) => self == t,
            // scAbs
            Term::Abs(v, t_inner) => self == &Term::Var(*v) || self.is_subterm(t_inner),
            // scApp
            Term::App(t1, t2) => self.is_subterm(t1) || self.is_subterm(t2),
        }
    }

    /// Is this term a proper subterm of `t`
    /// ```
    /// use chumsky::Parser;
    /// use comp31311_langs::lambda::Term;
    /// let app = Term::parser().parse(r"\x.y").unwrap();
    /// let nested = Term::parser().parse(r"\a.\b.\c.\x.y").unwrap();
    /// assert!(!app.is_proper_subterm(&app));
    /// assert!(app.is_proper_subterm(&nested));
    /// ```
    pub fn is_proper_subterm(&self, t: &Term) -> bool {
        self != t && self.is_subterm(t)
    }

    /// Return the set of all bound variables in this expression
    /// ```
    /// use chumsky::Parser;
    /// use comp31311_langs::lambda::{Term, Variable, Vars};
    /// use std::collections::HashSet;
    /// let t = Term::parser().parse(r"\a.\b.\c.def").unwrap();
    /// let s: Vars = "abc".chars().map(Variable::new).collect();
    /// assert_eq!(t.bv(), s);
    /// ```
    pub fn bv(&self) -> Vars {
        match self {
            // bcVarbv
            Term::Var(_) => Default::default(),
            // scAbsbv
            Term::Abs(v, t) => t.bv().union(*v),
            // scAppbv
            Term::App(t1, t2) => t1.bv().union(t2.bv()),
        }
    }

    /// Return the set of all free variables in this expression
    /// ```
    /// use chumsky::Parser;
    /// use comp31311_langs::lambda::{Term, Variable, Vars};
    /// use std::collections::HashSet;
    /// let t = Term::parser().parse(r"\a.\b.\c.def").unwrap();
    /// let s: Vars = "def".chars().map(Variable::new).collect();
    /// assert_eq!(t.fv(), s);
    /// ```
    pub fn fv(&self) -> Vars {
        match self {
            // bcVarfv
            Term::Var(v) => Vars::from(*v),
            // scAbsfv
            Term::Abs(v, t) => t.fv().difference(*v),
            // scAppfv
            Term::App(t1, t2) => t1.fv().union(t2.fv()),
        }
    }

    /// Return all the variables in a given Term
    /// ```
    /// use chumsky::Parser;
    /// use comp31311_langs::lambda::{Term, Variable, Vars};
    /// use std::collections::HashSet;
    /// let t = Term::parser().parse(r"\a.\b.\c.def").unwrap();
    /// let s: Vars = "abcdef".chars().map(Variable::new).collect();
    /// assert_eq!(t.vars(), s);
    /// ```
    pub fn vars(&self) -> Vars {
        self.fv().union(self.bv())
    }

    /// Renaming variable A to variable B in term
    /// ```
    /// use chumsky::Parser;
    /// use comp31311_langs::lambda::{Term, Variable};
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
    /// use comp31311_langs::lambda::{Term, Variable};
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
                let vs = self.vars().union(other.vars());
                let v = vs.freshv();
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
    /// use comp31311_langs::lambda::{Term, Variable};
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
                let vs: Vars = self.vars().union(t.vars()).union(x);
                let w = vs.freshv();
                Term::abs(w, u.ren(*y, w).cap_avoid_subst(x, t))
            }
            // scApp[]
            Term::App(r, s) => Term::app(r.cap_avoid_subst(x, t.clone()), s.cap_avoid_subst(x, t)),
        }
    }

    /// Perform one step of beta reduction if possible
    /// ```
    /// use chumsky::Parser;
    /// use comp31311_langs::lambda::{Term, Variable};
    /// let orig = Term::parser().parse(r"(\x.x)(\y.y)").unwrap();
    /// let correct = Term::parser().parse(r"\y.y").unwrap();
    /// assert_eq!(orig.beta_reduction(), Some(correct));
    /// ```
    pub fn beta_reduction(&self) -> Option<Term> {
        match self {
            // bcAppβ
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

    /// Compute the parallel reduct of the term
    /// ```
    /// use chumsky::Parser;
    /// use comp31311_langs::lambda::{Term, Variable};
    /// let orig = Term::parser().parse(r"(\s.s(s(z)))((\x.x)(\y.y))").unwrap();
    /// let correct = Term::parser().parse(r"(\y.y)((\y.y)z)").unwrap();
    /// assert!(orig.parallel_reduct().is_alpha_equivalent(&correct));
    /// ```
    pub fn parallel_reduct(&self) -> Term {
        match self {
            // bcVar◇
            Term::Var(_) => self.clone(),
            // scAbs◇
            Term::Abs(x, t) => Term::abs(*x, t.parallel_reduct()),
            // scApp◇
            Term::App(t, u) => match *t.clone() {
                Term::Abs(x, tp) => tp.parallel_reduct().cap_avoid_subst(x, u.parallel_reduct()),
                _ => Term::app(t.parallel_reduct(), u.parallel_reduct()),
            },
        }
    }

    /// Perform the parallel reduct operation n times
    /// ```
    /// use chumsky::Parser;
    /// use comp31311_langs::lambda::{Term, Variable};
    /// let orig = Term::parser().parse(r"(\s.s(s(z)))((\x.x)(\y.y))").unwrap();
    /// let correct = Term::parser().parse(r"z").unwrap();
    /// assert_eq!(orig.parallel_reduct_n(2), correct);
    /// ```
    pub fn parallel_reduct_n(&self, n: usize) -> Term {
        let mut t = self.clone();

        for _ in 0..n {
            let tr = t.parallel_reduct();

            // if we've stopped replacing stuff nothing new will happen
            if tr == t {
                break;
            }

            t = tr;
        }

        t
    }

    /// Find all the redexes in the term
    /// ```
    /// use std::collections::HashSet;
    /// use chumsky::Parser;
    /// use comp31311_langs::lambda::{Term, Variable};
    /// let orig = Term::parser().parse(r"(\y.y)a((\x.x)y)").unwrap();
    /// let correct = HashSet::from([
    ///     Term::parser().parse(r"(\x.x)y").unwrap(),
    ///     Term::parser().parse(r"(\y.y)a").unwrap(),
    /// ]);
    /// let owned = orig.redexes().into_iter().cloned().collect();
    /// assert_eq!(correct, owned);
    /// ```
    pub fn redexes(&self) -> Vec<&Term> {
        let mut redexes = vec![];
        let mut term_stack = vec![self];
        while let Some(term) = term_stack.pop() {
            match term {
                Term::Var(_) => {}
                Term::Abs(_, t) => {
                    term_stack.push(t);
                }
                Term::App(t1, t2) => {
                    term_stack.push(t1);
                    term_stack.push(t2);
                    if t1.is_abs() {
                        redexes.push(term);
                    }
                }
            }
        }

        redexes
    }

    /// Perform a beta-alpha reduction - a beta reduction where no new variables are introduced
    /// ```
    /// use chumsky::Parser;
    /// use comp31311_langs::lambda::{Term, Variable};
    /// let orig = Term::parser().parse(r"(\y.\x.y)z").unwrap();
    /// let correct = Term::parser().parse(r"\x.z").unwrap();
    /// assert_ne!(orig.beta_reduction().as_ref(), Some(&correct));
    /// assert_eq!(orig.beta_alpha_reduction(), Some(correct));
    /// ```
    pub fn beta_alpha_reduction(&self) -> Option<Term> {
        // capturing substitution
        fn subst(term: &Term, x: Variable, t: Term) -> Term {
            match term {
                // bcVar[]
                Term::Var(y) => {
                    if *y == x {
                        t
                    } else {
                        term.clone()
                    }
                }
                // scAbs[]
                Term::Abs(y, u) => {
                    let vs: Vars = term.vars().union(t.vars()).union(x);
                    let w = vs.freshv();
                    Term::abs(w, subst(&u.ren(*y, w), x, t)).ren(w, *y)
                }
                // scApp[]
                Term::App(r, s) => Term::app(subst(r, x, t.clone()), subst(s, x, t)),
            }
        }

        /// Perform the body of the beta-alpha reduction, without checking whether we are still alpha equivalent
        fn beta_alpha_reduction_inner(term: &Term) -> Option<Term> {
            match term {
                // bcAppβα
                Term::App(abs, a) if abs.is_abs() => match *abs.clone() {
                    Term::Abs(x, t) => Some(subst(&t, x, *a.clone())),
                    _ => None,
                },
                // scAbsβα
                Term::Abs(x, t) => beta_alpha_reduction_inner(t).map(|tp| Term::abs(*x, tp)),
                // scAppβα
                Term::App(t, u) => beta_alpha_reduction_inner(t)
                    .map(|tp| Term::app(tp, *u.clone()))
                    .or_else(|| beta_alpha_reduction_inner(u).map(|up| Term::app(*t.clone(), up))),
                // if its none of these cases it doesn't beta reduce
                _ => None,
            }
        }

        // compute the beta-alpha reduction and beta reduction, returning the beta-alpha reduction
        // if it is alpha equivalent to the beta reduced term
        let beta_alpha_reduced = beta_alpha_reduction_inner(self);
        let beta_reduced = self.beta_reduction();

        match (beta_alpha_reduced, beta_reduced) {
            (Some(bar), Some(br)) => bar.is_alpha_equivalent(&br).then_some(bar),
            _ => None,
        }
    }
}
