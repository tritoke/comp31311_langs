// Module to keep all of the formatting code in one place so it doesn't clutter up term.rs

use crate::lambda::Term;
use std::fmt;

// struct to hold the left hand side of an application while formatting
pub(super) struct Left(Term);

impl fmt::Display for Left {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.0 {
            Term::Var(v) => write!(f, "{v}"),
            Term::Abs(v, t) => write!(f, "(λ{v}.{t})"),
            Term::App(t1, t2) => {
                let l1 = Left(*t1.clone());
                let l2 = Left(*t2.clone());
                write!(f, "{l1}{l2}")
            }
        }
    }
}

// struct to hold the right hand side of an application while formatting
pub(super) struct Right(Term);

impl fmt::Display for Right {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.0 {
            Term::Var(v) => write!(f, "{v}"),
            Term::Abs(v, t) => write!(f, "λ{v}.{t}"),
            Term::App(t1, t2) => {
                let l = Left(*t1.clone());
                let r = Right(*t2.clone());
                write!(f, "({l}{r})")
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
                Term::App(t1, t2) => {
                    let l = Left(*t1.clone());
                    let r = Right(*t2.clone());
                    write!(f, "{l}{r}")
                }
            }
        }
    }
}
