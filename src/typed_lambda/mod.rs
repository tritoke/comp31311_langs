mod formatting;
mod preterm;
mod r#type;
mod type_environment;

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
}
