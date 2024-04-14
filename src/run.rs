use std::{cell::RefCell, collections::HashMap, rc::Rc, sync::Arc};

use crate::{ast::Statement, eval::{self, Value}};

#[derive(Clone, Debug)]
struct EnvInner {
    parent: Option<Env>,
    scope: HashMap<Arc<str>, eval::Value>,
}
impl PartialEq for EnvInner {
    fn eq(&self, other: &Self) -> bool {
        false
    }
}
#[derive(Clone, Debug, PartialEq)]
pub struct Env {
    inner: Rc<RefCell<EnvInner>>,
    n: usize,
}

impl Env {
    pub fn new() -> Self {
        Self {
            inner: Rc::new(RefCell::new(EnvInner {
                parent: None,
                scope: HashMap::new()
            })),
            n: 0
        }
    }
    pub fn assign(&mut self, name: &Arc<str>, val: eval::Value) {
        self.inner.borrow_mut().scope.insert(name.clone(), val);
    }
    pub fn find(&self, name: &str) -> Option<eval::Value> {
        let mut node = self.inner.clone();
        loop {
            node = match node.clone().as_ref().borrow().scope.get(name) {
                None => {
                    let Some(node_rc) = &node.as_ref().borrow().parent else {
                        return None;
                    };
                    node_rc.inner.clone()
                }
                some => {
                    return some.cloned();
                }
            }
        }
    }
    pub fn push_scope(&mut self) {
        self.n += 1;
        let parent = self.inner.clone();
        self.inner = Rc::new(RefCell::new(EnvInner {parent: Some(Env { inner: parent, n: 0 }) , scope: HashMap::new()}));
        
    }
    pub fn pop_scope(&mut self) {
        self.n -= 1;
        let parent = self.inner.as_ref().borrow().parent.clone().unwrap().inner;
        self.inner = parent;
    }
}

pub struct Interpreter {
    env: Env,
    prog: Vec<Statement>,
}

pub fn eval_statement(statement: &Statement, env: &mut Env) -> Option<Value> {
        match statement {
            Statement::Expression(e) => {
                eval::eval(e, env);
            },
            Statement::Print(e) => {
                let value = eval::eval(e, env);
                println!("{}", value);
            }
            Statement::Var(name, val) => {
                let v = eval::eval(val, env);
                env.assign(name, v);
            }
            Statement::Block(block) => {
                env.push_scope();
                for s in block {
                    match eval_statement(s, env) {
                        None => (),
                        some => {
                            env.pop_scope();
                            return some;
                        }
                    }
                }
                env.pop_scope();
            }
            Statement::Conditional(cond, do_if, do_else) => {
                match eval::eval(cond, env) {
                    eval::Value::Bool(c) => {
                        if c {
                            match eval_statement(&do_if, env) {
                                None => (),
                                some => return some,
                            }
                        } else {
                            if let Some(do_else) = do_else {
                                match eval_statement(&do_else, env) {
                                    None => (),
                                    some => return some,
                                }                            
                            }
                        }
                    }
                    _ => {
                        panic!("if statement codition is not a bool");
                    }
                }
            }
            Statement::FuncDef(name, args, body) => {
                env.assign(name, eval::Value::Func(args.clone(), body.clone(), env.clone()));
            }
            Statement::Return(e) => {
                let Some(e) = e else {
                    return Some(Value::Unit);
                };
                return Some(eval::eval(e, env));
            }
            Statement::While(e, body) => {
                let Statement::Block(body) = body.as_ref() else {
                    panic!();
                };
                while matches!(eval::eval(e, env), Value::Bool(true)) {
                    for stmt in body {
                        match eval_statement(&stmt, env) {
                            None => (),
                            some => return some,
                        }
                    }
                }
            }
        }
        return None;
}

impl Interpreter {
    pub fn new(prog: Vec<Statement>) -> Self {
        Self {
            env: Env::new(),
            prog,
        }
    }
    pub fn run(&mut self) {
        for statment in &self.prog {
            eval_statement(statment, &mut self.env);
        }
    }
}