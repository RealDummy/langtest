mod ast;
mod eval;
mod run;

fn main() {
    let input = std::fs::read_to_string(&std::path::PathBuf::new().with_file_name("src").join("prog.txt")).unwrap();
    let e = ast::expression(&mut ast::Tokenized::new(&input));
    
    println!("{:?}", eval::eval(&e));
}