use lambda_parser::lambda::*;

fn main() {
    let s = r"((\x.\y.z(xy))\y'.y'x)((\x.x)(yy'))";
    let t: Term = s.parse().unwrap();
    println!("{t}");
    println!("{t:#}");
}
