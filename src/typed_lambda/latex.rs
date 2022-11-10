use crate::lambda::latex::{ColourMapping, Depth, FormatLatex};

use crate::lambda::Variable;
use crate::typed_lambda::{
    formatting::{Left, Right},
    Preterm,
};
use std::collections::HashMap;

impl FormatLatex for Preterm {
    fn format_latex(&self, colours: &ColourMapping) -> String {
        let mut depths = HashMap::new();
        self.as_coloured_latex(colours, &mut depths)
    }
}

impl Preterm {
    fn as_coloured_latex(
        &self,
        colours: &ColourMapping,
        depths: &mut HashMap<Variable, Depth>,
    ) -> String {
        match self {
            Preterm::Var(x) => colours.colour_var(*x, depths.get(x).copied().unwrap_or_default()),
            Preterm::Abs(x, typ, t) => {
                let depth = *depths.entry(*x).and_modify(|n| *n += 1).or_insert(1);
                let s = format!(
                    r"\lambda {}:{typ}.{}",
                    colours.colour_var(*x, depth),
                    t.as_coloured_latex(colours, depths)
                );
                depths.insert(*x, depth - 1);
                s
            }
            Preterm::App(a, b) => format!(
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
            Left(Preterm::Var(x)) => {
                colours.colour_var(*x, depths.get(x).copied().unwrap_or_default())
            }
            Left(Preterm::Abs(x, typ, t)) => {
                let depth = *depths.entry(*x).and_modify(|n| *n += 1).or_insert(1);
                let s = format!(
                    r"(\lambda {}:{typ}.{})",
                    colours.colour_var(*x, depth),
                    t.as_coloured_latex(colours, depths)
                );
                depths.insert(*x, depth - 1);
                s
            }
            Left(Preterm::App(a, b)) => format!(
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
            Right(Preterm::Var(x)) => {
                colours.colour_var(*x, depths.get(x).copied().unwrap_or_default())
            }
            Right(Preterm::Abs(x, typ, t)) => {
                let depth = *depths.entry(*x).and_modify(|n| *n += 1).or_insert(1);
                let s = format!(
                    r"\lambda {}:{typ}.{}",
                    colours.colour_var(*x, depth),
                    t.as_coloured_latex(colours, depths)
                );
                depths.insert(*x, depth - 1);
                s
            }
            Right(Preterm::App(a, b)) => format!(
                "({}{})",
                Left(a).as_coloured_latex(colours, depths),
                Right(b).as_coloured_latex(colours, depths)
            ),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::lambda::{latex::ColourMapping, Variable};
    use crate::typed_lambda::Preterm;

    #[test]
    fn test_outputs_plain_latex_when_no_colours() {
        let t: Preterm = r"(\x:a.x)\y:b.y".parse().unwrap();
        let correct: String = r"(\lambda x:a.x)\lambda y:b.y".into();
        assert_eq!(t.format_latex(&ColourMapping::new()), correct);
    }

    #[test]
    fn test_handles_depth_correctly() {
        let t: Preterm = r"(\x:a.\x:b.y)\y:c.x".parse().unwrap();
        let mut colours = ColourMapping::new();
        colours.add(Variable::new('x'), 2, String::from("blue"));
        let correct: String = r"(\lambda x:a.\lambda \textcolor{blue}{x}:b.y)\lambda y:c.x".into();
        assert_eq!(t.format_latex(&colours), correct);
    }
}
