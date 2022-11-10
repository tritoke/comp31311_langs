use comp31311_langs::{colour_mapping, lambda::Term};
use std::collections::HashMap;

fn main() {
    let colours = colour_mapping! {
        "x" at depth 1 => "red",
        "x" at depth 2 => "pink",
        "y" at depth 1 => "blue",
        "z" at depth 0 => "green",
    };

    let terms: Vec<Term> = vec![
        r"z\x.x\y.yx\x.x".parse().unwrap(),
        r"\x.xz\x.xzyz".parse().unwrap(),
    ];

    for term in terms {
        println!("{}", term.format_latex(&colours));
    }
}
