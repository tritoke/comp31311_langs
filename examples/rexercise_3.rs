use comp31311_langs::lambda::{Term, Variable, Vars};

fn main() {
    println!("Part A.");
    let t: Term = r"\x.\y.xy".parse().unwrap();
    let st: Term = r"\y.xy".parse().unwrap();
    let fv_t = t.fv();
    let bv_t = t.bv();
    let fv_st = st.fv();
    let bv_st = st.bv();

    println!("term: {t}");
    println!("subterm: {st}");
    assert!(t.is_subterm(&st));

    println!("fv({t}) = {fv_t}");
    println!("bv({t}) = {bv_t}");
    println!("fv({st}) = {fv_st}");
    println!("bv({st}) = {bv_st}");
}
