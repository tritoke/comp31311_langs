mod formatting;
mod preterm;
mod r#type;
mod type_environment;

/// Module holding all the code related to formatting a Preterm using LaTeX
pub mod latex;

pub use preterm::Preterm;
pub use r#type::Type;
pub use type_environment::TypeEnvironment;

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! typ {
        ($t:literal) => {
            Type::raw($t)
        };
        ($s:expr => $t:expr) => {
            Type::function($s.into(), $t.into())
        };
        ($t:expr => $($ts:expr)=>+) => {
            Type::function($t, typ!($($ts)=>+))
        };
    }

    macro_rules! do_type_test {
        ($s:ident, $correct:ident) => {
            let parsed = $s.parse::<crate::typed_lambda::Type>().unwrap();
            assert_eq!(parsed, $correct);
            assert_eq!(format!("{}", parsed), $s);
        };
    }

    mod parsing {
        use super::*;
        use crate::lambda::Variable;

        #[test]
        fn test_raw_type() {
            let s = "x";
            let correct = typ!('x');
            do_type_test!(s, correct);
        }

        #[test]
        fn test_function_type() {
            let s = "x->y";
            let correct = typ!('x' => 'y');
            do_type_test!(s, correct);
        }

        #[test]
        fn test_function_type_with_brackets() {
            let s = "(x->y)->z";
            let correct = typ!(typ!('x' => 'y') => 'z');
            do_type_test!(s, correct);
        }

        #[test]
        fn test_function_type_association() {
            let s = "x->y->z";
            let correct = typ!(typ!('x') => typ!('y') => typ!('z'));
            do_type_test!(s, correct);
        }

        #[test]
        fn test_function_type_association_inside_brackets() {
            let s = "(a->b->c)->(d->e->f)->g->h->i";
            let correct = typ!(
                typ!(typ!('a') => typ!('b') => typ!('c')) =>
                typ!(typ!('d') => typ!('e') => typ!('f')) =>
                typ!(typ!('g') => typ!('h') => typ!('i'))
            );
            do_type_test!(s, correct);
        }

        #[test]
        fn test_parses_type_correctly() {
            let s = r"\x:a.y";
            let pt: Preterm = s.parse().unwrap();
            let correct = Preterm::abs(
                Variable::new('x'),
                typ!('a'),
                Preterm::Var(Variable::new('y')),
            );
            assert_eq!(correct, pt);
        }

        #[test]
        fn test_parses_nexted_type_correctly() {
            let s = r"\x:a.\x:b.y";
            let pt: Preterm = s.parse().unwrap();
            let correct = Preterm::abs(
                Variable::new('x'),
                typ!('a'),
                Preterm::abs(
                    Variable::new('x'),
                    typ!('b'),
                    Preterm::var(Variable::new('y')),
                ),
            );
            assert_eq!(correct, pt);
        }

        #[test]
        fn test_parses_nested_application_correctly() {
            let s = r"(\x:a.x)\y:b.y";
            let pt: Preterm = s.parse().unwrap();
            let correct = Preterm::app(
                Preterm::abs(
                    Variable::new('x'),
                    typ!('a'),
                    Preterm::var(Variable::new('x')),
                ),
                Preterm::abs(
                    Variable::new('y'),
                    typ!('b'),
                    Preterm::var(Variable::new('y')),
                ),
            );
            assert_eq!(correct, pt);
        }
    }

    mod formatting {
        use super::*;

        #[test]
        fn test_function_source_bracketed() {
            let s = "(a->b)->c";
            let correct = typ!(typ!('a' => 'b') => 'c');
            do_type_test!(s, correct);
        }

        #[test]
        fn test_function_nested_source_bracketed() {
            let s = "((a->b)->c)->((d->e)->f)->g";
            let correct = typ!(
                typ!(typ!('a' => 'b') => 'c') =>
                typ!(typ!('d' => 'e') => 'f') =>
                'g'
            );
            do_type_test!(s, correct);
        }

        #[test]
        fn test_function_alternate_formatting_simple() {
            let s = "a->b->c->d->e";
            let correct = "(a->(b->(c->(d->e))))";
            let st: Type = s.parse().unwrap();
            let ct: Type = correct.parse().unwrap();
            assert_eq!(st, ct);
            assert_eq!(format!("{ct:#}"), correct);
        }

        #[test]
        fn test_function_alternate_formatting_complex() {
            let s = "((a->b)->c)->((d->e)->f)->g";
            let correct = "(((a->b)->c)->(((d->e)->f)->g))";
            let st: Type = s.parse().unwrap();
            let ct: Type = correct.parse().unwrap();
            assert_eq!(st, ct);
            assert_eq!(format!("{ct:#}"), correct);
        }
    }

    mod functionality {
        use super::*;

        #[test]
        fn test_type_inference_cannot_infer_free_variable() {
            let pt: Preterm = "x".parse().unwrap();
            let inferred_type = TypeEnvironment::new().infer_type(&pt);
            assert!(inferred_type.is_none());
        }

        #[test]
        fn test_type_inference_infers_function_type() {
            let pt: Preterm = r"\x:s.x".parse().unwrap();
            let correct: Type = "s->s".parse().unwrap();
            let inferred_type = TypeEnvironment::new().infer_type(&pt);
            assert_eq!(inferred_type.unwrap(), correct);
        }
    }
}
