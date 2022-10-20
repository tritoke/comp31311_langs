use comp31311_langs::lambda::{Term, Vars};

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
    assert!(st.is_subterm(&t));

    println!("fv({t}) = {fv_t}");
    println!("bv({t}) = {bv_t}");
    println!("fv({st}) = {fv_st}");
    println!("bv({st}) = {bv_st}");

    println!("{bv_st} ⊆ {bv_t}");
    assert!(bv_st.is_subset(&bv_t));
    println!("{fv_st} ⊈ {fv_t}");
    assert!(!fv_st.is_subset(&fv_t));

    println!("\nPart B.");
    let t: Term = r"x\x.x".parse().unwrap();
    println!("term: {t}");

    let fv_t = t.fv();
    let bv_t = t.bv();
    println!("fv({t}) = {fv_t}");
    println!("bv({t}) = {bv_t}");

    let int = t.fv().intersection(t.bv());
    println!("{} ^ {} = {} != {{}}", t.fv(), t.bv(), int);
    assert_ne!(int, Vars::new());
}
