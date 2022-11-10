mod colour_mapping;
pub use colour_mapping::{Colour, ColourMapping, Depth};

use crate::lambda::{
    formatting::{Left, Right},
    Term, Variable,
};
use std::collections::HashMap;

impl Term {
    /// Format a term using latex and a list of colours
    pub fn format_latex(&self, colours: &ColourMapping) -> String {
        let mut depths = HashMap::new();
        self.as_coloured_latex(colours, &mut depths)
    }

    fn as_coloured_latex(
        &self,
        colours: &ColourMapping,
        depths: &mut HashMap<Variable, Depth>,
    ) -> String {
        match self {
            Term::Var(x) => colours.colour_var(*x, depths.get(x).copied().unwrap_or_default()),
            Term::Abs(x, t) => {
                let depth = *depths.entry(*x).and_modify(|n| *n += 1).or_insert(1);
                let s = format!(
                    r"\lambda {}.{}",
                    colours.colour_var(*x, depth),
                    t.as_coloured_latex(colours, depths)
                );
                depths.insert(*x, depth - 1);
                s
            }
            Term::App(a, b) => format!(
                "{}{}",
                Left(a).as_coloured_latex(colours, depths),
                Right(b).as_coloured_latex(colours, depths),
            ),
        }
    }
}

impl<'a> Left<'a> {
    fn as_coloured_latex(
        &self,
        colours: &ColourMapping,
        depths: &mut HashMap<Variable, Depth>,
    ) -> String {
        match self {
            Left(Term::Var(x)) => {
                colours.colour_var(*x, depths.get(x).copied().unwrap_or_default())
            }
            Left(Term::Abs(x, t)) => {
                let depth = *depths.entry(*x).and_modify(|n| *n += 1).or_insert(1);
                let s = format!(
                    r"(\lambda {}.{})",
                    colours.colour_var(*x, depth),
                    t.as_coloured_latex(colours, depths)
                );
                depths.insert(*x, depth - 1);
                s
            }
            Left(Term::App(a, b)) => format!(
                "{}{}",
                Left(a).as_coloured_latex(colours, depths),
                Left(b).as_coloured_latex(colours, depths),
            ),
        }
    }
}

impl<'a> Right<'a> {
    fn as_coloured_latex(
        &self,
        colours: &ColourMapping,
        depths: &mut HashMap<Variable, usize>,
    ) -> String {
        match self {
            Right(Term::Var(x)) => {
                colours.colour_var(*x, depths.get(x).copied().unwrap_or_default())
            }
            Right(Term::Abs(x, t)) => {
                let depth = *depths.entry(*x).and_modify(|n| *n += 1).or_insert(1);
                let s = format!(
                    r"\lambda {}.{}",
                    colours.colour_var(*x, depth),
                    t.as_coloured_latex(colours, depths)
                );
                depths.insert(*x, depth - 1);
                s
            }
            Right(Term::App(a, b)) => format!(
                "({}{})",
                Left(a).as_coloured_latex(colours, depths),
                Right(b).as_coloured_latex(colours, depths)
            ),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::lambda::{latex::ColourMapping, Term, Variable};

    #[test]
    fn test_outputs_plain_latex_when_no_colours() {
        let t: Term = r"(\x.x)\y.y".parse().unwrap();
        let correct: String = r"(\lambda x.x)\lambda y.y".into();
        assert_eq!(t.format_latex(&ColourMapping::new()), correct);
    }

    #[test]
    fn test_handles_depth_correctly() {
        let t: Term = r"(\x.\x.y)\y.x".parse().unwrap();
        let mut colours = ColourMapping::new();
        colours.add(Variable::new('x'), 2, String::from("blue"));
        let correct: String = r"(\lambda x.\lambda \textcolor{blue}{x}.y)\lambda y.x".into();
        assert_eq!(t.format_latex(&colours), correct);
    }
}
