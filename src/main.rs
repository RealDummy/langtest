mod ast;
mod eval;
mod run;

fn main() {
    let input = std::fs::read_to_string(&std::path::PathBuf::new().with_file_name("src").join("prog.txt")).unwrap();
    let prog = ast::parse(&mut ast::Tokenized::new(&input));
    let mut i = run::Interpreter::new(prog);
    i.run();
}