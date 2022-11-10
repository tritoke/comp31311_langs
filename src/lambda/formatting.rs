// Module to keep all of the formatting code in one place so it doesn't clutter up term.rs

use crate::lambda::Term;
use std::fmt;

// struct to hold the left hand side of an application while formatting
pub(super) struct Left<'a>(pub &'a Term);

impl<'a> fmt::Display for Left<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.0 {
            Term::Var(v) => write!(f, "{v}"),
            Term::Abs(v, t) => write!(f, "(λ{v}.{t})"),
            Term::App(t1, t2) => write!(f, "{}{}", Left(t1), Left(t2)),
        }
    }
}

// struct to hold the right hand side of an application while formatting
pub(super) struct Right<'a>(pub &'a Term);

impl<'a> fmt::Display for Right<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.0 {
            Term::Var(v) => write!(f, "{v}"),
            Term::Abs(v, t) => write!(f, "λ{v}.{t}"),
            Term::App(t1, t2) => write!(f, "({}{})", Left(t1), Right(t2)),
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
                    let l = Left(t1);
                    let r = Right(t2);
                    write!(f, "{l}{r}")
                }
            }
        }
    }
}
