use crate::lambda::Variable;
use std::collections::HashMap;

/// The depth at which a variable should be highlighted
/// depth 0 = when it is free / unbound
/// depth 1 = on the first time it is defined in a bound context, e.g. \x.x\x.x would be \<x>.<x>\x.x
///           if <x> represents colouring x and with a depth of 1
/// depth n = the nth nesting level
pub type Depth = usize;

/// The colour to output when formatting the variable, this can be any string so you can use your
/// own custom colours which you define in your LaTeX document
pub type Colour = String;

/// This struct understands how to colour a variable based on what depth it is seen at
#[derive(Default, Debug, Clone, Eq, PartialEq)]
pub struct ColourMapping {
    mapping: HashMap<(Variable, Depth), Colour>,
}

impl ColourMapping {
    /// Create a new empty colour mapping
    pub fn new() -> Self {
        Default::default()
    }

    /// Remove a variable + depth pair from the mapping
    pub fn remove(&mut self, var: Variable, depth: Depth) {
        self.mapping.remove(&(var, depth));
    }

    /// Add a variable + depth pair to the mapping
    pub fn add(&mut self, var: Variable, depth: Depth, colour: Colour) {
        self.mapping.insert((var, depth), colour);
    }

    /// Return the colour to use for a given variable + depth pair, if one has been assigned
    pub fn colour_var(&self, var: Variable, depth: Depth) -> String {
        if let Some(colour) = self.mapping.get(&(var, depth)).cloned() {
            format!(r"\textcolor{{{colour}}}{{{var}}}")
        } else {
            format!("{var}")
        }
    }
}

impl FromIterator<((Variable, Depth), Colour)> for ColourMapping {
    fn from_iter<T: IntoIterator<Item = ((Variable, Depth), Colour)>>(iter: T) -> Self {
        Self {
            mapping: iter.into_iter().collect(),
        }
    }
}

impl<const N: usize> From<[((Variable, Depth), Colour); N]> for ColourMapping {
    fn from(mapping: [((Variable, Depth), Colour); N]) -> Self {
        mapping.into_iter().collect()
    }
}

/// Macro to construct a ColourMapping intuitively and simply
#[macro_export]
macro_rules! colour_mapping {
    ($($variable:literal at depth $depth:literal => $colour:literal),* $(,)?) => {
        comp31311_langs::lambda::latex::ColourMapping::from([
            $(
                (
                    ($variable.parse::<comp31311_langs::lambda::Variable>().unwrap(), $depth),
                    $colour.to_string()
                )
            ),*
        ])
    };
}
