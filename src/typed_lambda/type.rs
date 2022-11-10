use crate::lambda::Variable;
use chumsky::{error::Simple, prelude::*, Parser};
use std::fmt;

/// enum representing all the different ways we can construct a type
#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub enum Type {
    /// a raw type e.g. σ
    Raw(char),
    /// a function type, e.g. σ -> τ
    Function(Box<Type>, Box<Type>),
}

impl From<char> for Type {
    fn from(c: char) -> Self {
        Type::raw(c)
    }
}

impl Type {
    /// The subset of the greek alphabet I'm allowing to be used in types
    /// note: the english alphabet is also allowed
    pub const GREEK_ALPHABET: &'static str = "αβγδεζηθικλμνξοπρστυφχψωΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩ";

    /// Create a raw type from a character
    /// ```
    /// use comp31311_langs::typed_lambda::Type;
    /// assert_eq!(Type::raw('a'), Type::Raw('a'));
    /// ```
    pub fn raw(t: char) -> Type {
        // note: this assertion is so that ,:@~ etc are not valid type names
        assert!(
            t.is_alphabetic() || Type::GREEK_ALPHABET.find(t).is_some(),
            "Only characters from the english or greek alphabets are valid type names."
        );
        Type::Raw(t)
    }

    /// Createa a new function type from two other types
    /// ```
    /// use comp31311_langs::typed_lambda::Type;
    /// let a = Type::raw('a');
    /// let b = Type::raw('b');
    /// assert_eq!(Type::Function(Box::new(a.clone()), Box::new(b.clone())), Type::function(a, b))
    /// ```
    pub fn function(source: Type, target: Type) -> Type {
        Type::Function(Box::new(source), Box::new(target))
    }

    /// is the type a raw type
    /// ```
    /// use comp31311_langs::typed_lambda::Type;
    /// let a = Type::raw('a');
    /// assert!(a.is_raw());
    /// ```
    pub fn is_raw(&self) -> bool {
        matches!(self, Type::Raw(_))
    }

    /// is the type a function type
    /// ```
    /// use comp31311_langs::typed_lambda::Type;
    /// let a = Type::raw('a');
    /// let b = Type::raw('b');
    /// let f = Type::function(a, b);
    /// assert!(f.is_function());
    /// ```
    pub fn is_function(&self) -> bool {
        matches!(self, Type::Function(_, _))
    }

    /// Chumsky parser for the Type type
    /// ```
    /// use chumsky::Parser;
    /// use comp31311_langs::typed_lambda::Type;
    /// let t = Type::parser().parse("(a->b)->(σ->τ)").unwrap();
    /// let [a, b, sigma, tau] = ['a', 'b', 'σ', 'τ'].map(Type::raw);
    /// let correct = Type::function(Type::function(a, b), Type::function(sigma, tau));
    /// assert_eq!(t, correct);
    /// ```
    pub fn parser() -> impl Parser<char, Type, Error = Simple<char>> + Clone {
        recursive(|typ| {
            let plain_var = one_of(Type::GREEK_ALPHABET)
                .or(one_of(Variable::ALPHABET))
                .map(Type::raw);
            let bracketed_type = typ.delimited_by(just('('), just(')'));
            let atom = plain_var.clone().or(bracketed_type.clone());

            // function
            atom.clone()
                .then_ignore(just("->"))
                .repeated()
                .then(atom)
                .foldr(Type::function)
        })
    }
}

impl std::str::FromStr for Type {
    type Err = Vec<Simple<char>>;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Type::parser().parse(s)
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if f.alternate() {
            match self {
                Type::Raw(r) => write!(f, "{r}"),
                Type::Function(s, t) => write!(f, "({s:#}->{t:#})"),
            }
        } else {
            match self {
                Type::Raw(r) => write!(f, "{r}"),
                Type::Function(s, t) => {
                    if s.is_function() {
                        write!(f, "({s})")?;
                    } else {
                        write!(f, "{s}")?;
                    }
                    write!(f, "->{t}")
                }
            }
        }
    }
}
