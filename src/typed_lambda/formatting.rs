// Module to keep all of the formatting code in one place so it doesn't clutter up term.rs

use crate::typed_lambda::Preterm;
use std::fmt;

// struct to hold the left hand side of an application while formatting
pub(super) struct Left<'a>(&'a Preterm);

impl<'a> fmt::Display for Left<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.0 {
            Preterm::Var(v) => write!(f, "{v}"),
            Preterm::Abs(v, ty, t) => write!(f, "(λ{v}:{ty}.{t})"),
            Preterm::App(t1, t2) => {
                let l1 = Left(t1);
                let l2 = Left(t2);
                write!(f, "{l1}{l2}")
            }
        }
    }
}

// struct to hold the right hand side of an application while formatting
pub(super) struct Right<'a>(&'a Preterm);

impl<'a> fmt::Display for Right<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.0 {
            Preterm::Var(v) => write!(f, "{v}"),
            Preterm::Abs(v, ty, t) => write!(f, "λ{v}:{ty}.{t}"),
            Preterm::App(t1, t2) => {
                let l = Left(t1);
                let r = Right(t2);
                write!(f, "({l}{r})")
            }
        }
    }
}

impl fmt::Display for Preterm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if f.alternate() {
            // in the alternate representation fully bracket the expression
            match self {
                Preterm::Var(v) => write!(f, "{v:#}"),
                Preterm::Abs(v, ty, t) => write!(f, "(λ{v:#}:{ty:#},{t:#})"),
                Preterm::App(t1, t2) => write!(f, "({t1:#}{t2:#})"),
            }
        } else {
            match self {
                Preterm::Var(v) => write!(f, "{v}"),
                Preterm::Abs(v, ty, t) => write!(f, "λ{v}:{ty}.{t}"),
                Preterm::App(t1, t2) => {
                    let l = Left(t1);
                    let r = Right(t2);
                    write!(f, "{l}{r}")
                }
            }
        }
    }
}
