mod ast;
mod eval;
mod run;

fn main() {
    let input = std::fs::read_to_string(&std::path::PathBuf::new().with_file_name("prog").join(format!("{}.txt", std::env::args().skip(1).next().unwrap_or("prog".into())))).unwrap();
    let prog = ast::parse(&mut ast::Tokenized::new(&input));
    println!("{:?}", prog);
    let mut i = run::Interpreter::new(prog);
    i.run();
}