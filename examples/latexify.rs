use clap::Parser;
use comp31311_langs::{
    lambda::{
        latex::{ColourMapping, FormatLatex},
        Term, Variable,
    },
    typed_lambda::Preterm,
};

#[derive(Debug, Parser, Clone)]
struct Args {
    /// The term to output in colour
    #[arg(short, long)]
    term: Option<String>,

    /// The preterm to output in colour
    #[arg(short, long, conflicts_with = "term")]
    preterm: Option<String>,

    /// The colours to use to format the term
    /// format: <variable>:<depth>:<colour>
    /// e.g. -c x:1:red to format the variable x red when it occurs at depth 1.
    #[arg(short = 'c', long = "colour")]
    mapping: Vec<String>,

    /// output an example LaTeX document called out.tex to show the rendered term immediately
    #[arg(short, long = "output", default_value_t = false)]
    output_document: bool,
}

#[rustfmt::skip]
fn main() {
    let args: Args = Args::parse();

    let mut colour_map = ColourMapping::new();
    
    for mapping in args.mapping {
        let parts: Vec<_> = mapping.split(':').collect();
        if parts.len() < 3 {
            eprintln!("Mapping {mapping:?} has invalid format, should be: <variable>:<depth>:<colour>");
            continue;
        }
        
        let var = parts[0].parse::<Variable>();
        let depth = parts[1].parse::<usize>();
        let colour = parts[2].to_string();
        
        if colour.is_empty() {
            eprintln!("Mapping {mapping:?} contains an empty colour, this is probably a mistake.")
        }

        match (var, depth) {
            (Ok(v), Ok(d)) => colour_map.add(v, d, colour),
            (Err(e), _) => eprintln!("Mapping {mapping:?} has invalid variable name - {:?} - {e:?}", parts[0]),
            (_, Err(e)) => eprintln!("Mapping {mapping:?} has invalid depth - {:?} - {e:?}", parts[1]),
        }
    }
    
    let latex = if let Some(term) = args.term {
        let t: Term = term.parse().expect("Failed to parse term.");
        t.format_latex(&colour_map)
    } else if let Some(preterm) = args.preterm {
        let pt: Preterm = preterm.parse().expect("Failed to parse preterm.");
        pt.format_latex(&colour_map)
    } else {
        panic!("Either --term or --preterm must be specified.");
    };

    println!("{latex}");
    if args.output_document {
        std::fs::write(
            "out.tex",
            format!("\
            \\documentclass{{article}}\n\
            \\usepackage{{xcolor}}\n\
            \\begin{{document}}\n\
            ${latex}$\n\
            \\end{{document}}\n\
            "),
        )
        .expect("Failed to write to `out.tex`.");
    }
}
