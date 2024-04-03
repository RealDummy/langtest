mod ast;
mod eval;

fn main() {
    let input = r#"1+2 + "foo""#;
    let e = ast::expression(&mut ast::Tokenized::new(input));
    
    println!("{:?}", eval::eval(&e));
}