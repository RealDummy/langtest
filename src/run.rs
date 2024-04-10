use std::{collections::HashMap, sync::Arc};

use crate::{ast::{Expr, Statement, Token}, eval};

pub struct Env {
    vars: HashMap<Arc<str>, eval::Value>
}

impl Env {
    pub fn new() -> Self {
        Self {
            vars: HashMap::new()
        }
    }
    pub fn assign(&mut self, name: &Arc<str>, val: eval::Value) {
        self.vars.insert(name.clone(), val);
    }
    pub fn find(&self, name: &str) -> Option<eval::Value> {
        self.vars.get(name).cloned()
    }
}

pub struct Interpreter {
    env: Env,
    prog: Vec<Statement>,
}

impl Interpreter {
    pub fn new(prog: Vec<Statement>) -> Self {
        Self {
            env: Env::new(),
            prog
        }
    }
    pub fn run(&mut self) {
        for statment in &self.prog {
            match statment {
                Statement::Expression(_) => (),
                Statement::Print(e) => {
                    let value = eval::eval(e, &self.env);
                    println!("{:?}", value);
                }
                Statement::Var(name, val) => {
                    self.env.assign(name, eval::eval(val, &self.env));
                }
            }
        }
    }
}