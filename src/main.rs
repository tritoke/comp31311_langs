use lambda_parser::lambda::Term;

fn main() {
    let s = "λw.w(λx.λy.λx.xbx)λx.x(λx.x)awy";
    let t: Term = s.parse().unwrap();
    println!("{}", t);
    println!("{:#}", t);
}
