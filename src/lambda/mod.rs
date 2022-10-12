mod formatting;
mod term;
mod variable;
mod vars;

pub use term::Term;
pub use variable::Variable;
pub use vars::Vars;

#[cfg(test)]
mod tests {
    use super::*;
    use chumsky::Parser;

    macro_rules! vars {
        ($($name:ident)*) => {
            #[allow(unused_parens)]
            let (
                $($name),*
            ) = (
                $(crate::lambda::Variable::parser().parse(stringify!($name)).unwrap()),*
            );
        };
        (primes = $primes:literal $($name:literal)*) => {
            (
                $(crate::lambda::Variable::new_with_primes($name, $primes)),*
            )
        };
    }

    macro_rules! term {
        (var $v:expr) => {
            crate::lambda::Term::Var($v)
        };
        (vars $($v:expr)*) => {
            (
                $( crate::lambda::Term::Var($v) ),*
            )
        };
        (app $($ts:expr)*) => {{
            let v = Vec::from([
                $($ts.clone().into()),*
            ]);
            v.into_iter().reduce(crate::lambda::Term::app).unwrap()
        }};
        (abs $v:expr => $t:expr) => {
            crate::lambda::Term::abs($v, $t.clone().into())
        };
    }

    /// macro to perform both the parsing and formatting tests
    macro_rules! do_test {
        ($s:ident, $correct:ident) => {{
            let parsed = $s.parse::<crate::lambda::Term>().unwrap();
            assert_eq!(parsed, $correct);
            assert_eq!(format!("{}", parsed), $s);
        }};
    }

    mod parsing {
        use super::*;

        #[test]
        fn test_variable_parser() {
            for (var, prim) in [("x", 0), ("x'", 1), ("x''", 2)] {
                let v = Variable::parser().parse(var).unwrap();
                assert_eq!(v, Variable::new_with_primes('x', prim))
            }
        }

        #[test]
        fn test_basic_var() {
            let s = "x";
            let correct: Term = Variable::new('x').into();
            do_test!(s, correct);
        }

        #[test]
        fn test_basic_var_prime() {
            let s = "x'";
            let correct: Term = Variable::new_with_primes('x', 1).into();
            do_test!(s, correct);
        }

        #[test]
        fn test_basic_app() {
            let s = "xy";
            vars!(x y);
            let correct = term!(app x y);
            do_test!(s, correct);
        }

        #[test]
        fn test_basic_abs() {
            let s = "λx.y";
            vars!(x y);
            let correct = term!(abs x => y);
            do_test!(s, correct);
        }

        #[test]
        fn test_app_chain_short() {
            let s = "abc";
            vars!(a b c);
            let correct = term!(app a b c);
            do_test!(s, correct);
        }

        #[test]
        fn test_app_chain() {
            let s = "abcdef";
            vars!(a b c d e f);
            let correct = term!(app a b c d e f);
            do_test!(s, correct);
        }

        #[test]
        fn test_abs_chain() {
            let s = "λx.λy.λz.xyz";
            vars!(x y z);
            let correct = term!(abs x => term!(abs y => term!(abs z => term!(app x y z))));
            do_test!(s, correct);
        }

        #[test]
        fn test_abs_after_var() {
            let s = "xλx.x";
            vars!(x);
            let correct = term!(app x term!(abs x => x));
            do_test!(s, correct);
        }

        #[test]
        fn test_worksheet1() {
            let s = "λw.w(λx.λy.λx.xbx)λx.x(λx.x)awy";

            vars!(a b w x y);
            let correct = term!(abs w => 
            term!(app
                term!(app w term!(abs x => term!(abs y => term!(abs x => term!(app x b x)))))
                term!(abs x => term!(app x term!(abs x => x) a w y))));

            do_test!(s, correct);
        }

        #[test]
        fn test_nested_bracketing() {
            let s = "(λx.x)(λy.y)λz.z";
            vars!(x y z);
            let correct = term!(app term!(abs x => x) term!(abs y => y) term!(abs z => z));
            do_test!(s, correct);
        }

        #[test]
        fn test_allows_mixed_brackets() {
            let s = "a<b(c[de])>";
            vars!(a b c d e);
            let correct = term!(app a term!(app b term!(app c term!(app d e))));
            assert_eq!(s.parse::<Term>().unwrap(), correct);
        }
    }

    mod formatting {
        use super::*;

        #[test]
        fn test_right_app_bracketed() {
            let s = "ab(cd)";
            vars!(a b c d);
            let correct = term!(app a b term!(app c d));
            do_test!(s, correct);
        }

        #[test]
        fn test_many_app() {
            let s = "abcdef";
            vars!(a b c d e f);
            let correct = term!(app a b c d e f);
            do_test!(s, correct);
        }

        #[test]
        fn test_many_abs() {
            let s = "λa.λb.λc.λd.λe.λf.abcdef";
            vars!(a b c d e f);
            let correct = term!(abs a => term!(abs b => term!(abs c =>
                term!(abs d => term!(abs e => term!(abs f => term!(app a b c d e f)
            ))))));
            do_test!(s, correct);
        }

        #[test]
        fn test_abs_nested_in_app_bracketed() {
            let s = r"(λx.x)(λy.y)λz.z";
            vars!(x y z);
            let correct = term!(app term!(abs x => x) term!(abs y => y) term!(abs z => z));
            do_test!(s, correct);
        }

        #[test]
        fn test_weird_expr_bracketing() {
            let s = "(λx.λy.z(xy))(λy'.y'x)((λx.x)(yy'))";
            vars!(x y z);
            let yp = vars!(primes = 1 'y');
            let correct = term!(app
            term!(app 
                term!(abs x => term!(abs y => term!(app z term!(app x y)))) 
                term!(abs yp => term!(app yp x)))
            term!(app term!(abs x => x) term!(app y yp)));
            do_test!(s, correct);
        }
    }

    mod functionality {
        use super::*;
        use std::collections::HashSet;

        macro_rules! parse {
            ($s:literal) => {
                $s.parse::<crate::lambda::Term>().unwrap()
            };
        }

        #[test]
        fn test_is_subterm_self() {
            let t = parse!("x");
            assert!(t.is_subterm(&t));
        }

        #[test]
        fn test_is_subterm_abs() {
            let t = parse!(r"\x.y");
            let x = parse!("x");
            let y = parse!("y");
            assert!(t.is_subterm(&x));
            assert!(t.is_subterm(&y));
        }

        #[test]
        fn test_is_subterm_abs_var() {
            let t = parse!(r"xx'");
            let x = parse!("x");
            let xp = parse!("x'");
            assert!(t.is_subterm(&x));
            assert!(t.is_subterm(&xp));
        }

        #[test]
        fn test_is_subterm_abs_very_nested() {
            let st = parse!(r"\x.y");
            let t = parse!(r"\a.\b.\c.\d.\e.\f.\g.\x.y");
            assert!(t.is_subterm(&st));
        }

        #[test]
        fn test_is_proper_subterm_rejects_equal() {
            let t = parse!(r"\x.y");
            assert!(!t.is_proper_subterm(&t));
        }

        #[test]
        fn test_vars() {
            let t = parse!(r"\a.\b.\c.defgh");
            vars!(a b c d e f g h);
            let s = Vars::from([a, b, c, d, e, f, g, h]);
            assert_eq!(t.vars(), s);
        }

        #[test]
        fn test_bv() {
            let t = parse!(r"(\a.\b.ab)\c.cdefg");
            vars!(a b c);
            let s = Vars::from([a, b, c]);
            assert_eq!(t.bv(), s);
        }

        #[test]
        fn test_fv() {
            let t = parse!(r"(\a.\b.de)\c.cfg");
            vars!(d e f g);
            let s = Vars::from([d, e, f, g]);
            assert_eq!(t.fv(), s);
        }

        #[test]
        fn test_ren() {
            let t = Term::parser().parse(r"\a.\b.\c.abc").unwrap();
            vars!(a b);
            let correct = Term::parser().parse(r"\b.\b.\c.bbc").unwrap();
            assert_eq!(t.ren(a, b), correct);
        }

        #[test]
        fn test_freshv() {
            vars!(a b);
            let s: Vars = [a].into();
            assert_eq!(s.freshv(), b);
            let s: Vars = [b].into();
            assert_eq!(s.freshv(), a);
            let s: Vars = Variable::ALPHABET.chars().map(Variable::new).collect();
            assert_eq!(s.freshv(), Variable::new_with_primes('a', 1));
        }

        #[test]
        fn test_alpha_equivalence() {
            let t1 = parse!(r"\w.w(\x.\y.\x.xbx)\x.x(\x.x)awy");
            let t2 = parse!(r"\p.p(\f.\g.\f.fbf)\m.m(\n.n)apy");

            assert!(t1.is_alpha_equivalent(&t2));
        }

        #[test]
        fn test_capture_avoiding_substitution1() {
            let t = parse!(r"\x.xy");
            let a = parse!(r"\z.z");
            vars!(y);

            let correct = parse!(r"\w.w\z.z");
            assert!(correct.is_alpha_equivalent(&t.cap_avoid_subst(y, a)));
        }

        #[test]
        fn test_capture_avoiding_substitution2() {
            let t = parse!(r"\x.xy\z.z");
            let a = parse!(r"fx");
            vars!(y);

            let correct = parse!(r"\w.w(fx)\z.z");
            assert!(correct.is_alpha_equivalent(&t.cap_avoid_subst(y, a)));
        }

        #[test]
        fn test_beta_reduction() {
            let t = parse!(r"((\x.\y.yx)a)(\z.(\v.z)b)");
            let steps = [
                parse!(r"(\y.ya)(\z.(\v.z)b)"),
                parse!(r"(\z.(\v.z)b)a"),
                parse!(r"(\v.a)b"),
                parse!(r"a"),
            ];

            let mut reduced = t.clone();
            for step in steps {
                reduced = reduced.beta_reduction().unwrap();
                assert!(reduced.is_alpha_equivalent(&step));
            }

            assert!(reduced.beta_reduction().is_none());
        }

        #[test]
        fn test_parallel_reduct() {
            let t = parse!(r"(<\p.(\f.[p(\x.c)][p(\y.y)])>(\g.g[(\z.\a.a)b])\w.wb)");
            let correct = parse!(r"(\f.[(\y.y(\a.a))(\x.c)][(\g.g(\a.a))(\y.y)])\w.wb");
            assert!(t.parallel_reduct().is_alpha_equivalent(&correct));
        }

        #[test]
        fn test_parallel_reduct_n() {
            let t = parse!(r"(<\p.(\f.[p(\x.c)][p(\y.y)])>(\g.g[(\z.\a.a)b])\w.wb)");
            let correct = parse!(r"(λd.c)(λc.c)((λc.c)λc.c)");
            assert!(t.parallel_reduct_n(2).is_alpha_equivalent(&correct));
        }
    }
}
