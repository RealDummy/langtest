use std::{cell::RefCell, collections::{HashMap, HashSet}, fmt::Debug, rc::Rc, sync::Arc};

use crate::{ast::Statement, eval::{self, Value}};

#[derive(Clone, Debug)]
pub struct EnvInner {
    parent: Option<Env>,
    scope: HashMap<Arc<str>, eval::Value>,
}
impl PartialEq for EnvInner {
    fn eq(&self, _other: &Self) -> bool {
        false
    }
}

#[derive(Clone, PartialEq)]
pub struct Env {
    inner: Rc<RefCell<EnvInner>>,
    refs: HashSet<Arc<str>>,
    ref_inner: Option<Rc<RefCell<EnvInner>>>,
    n: usize,
}
impl Debug for Env {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}", self.n))
    }
}

impl Env {
    pub fn new() -> Self {
        Self {
            inner: Rc::new(RefCell::new(EnvInner {
                parent: None,
                scope: HashMap::new(),
            })),
            n: 0,
            refs: HashSet::new(),
            ref_inner: None,
        }
    }
    fn closure(&mut self) -> Self {
        let parent_env = self.inner().parent.clone();
        let mut shallow_env = Env::new();
        shallow_env.inner().parent = parent_env;
        shallow_env.refs.extend(self.inner().scope.keys().cloned());
        shallow_env.ref_inner = Some(self.inner.clone());
        return shallow_env;
    }
    fn inner<'a>(&'a mut self)-> std::cell::RefMut<'a, EnvInner> {
        self.inner.as_ref().borrow_mut()
    }
    pub fn assign(&mut self, name: &Arc<str>, val: eval::Value) {
        let None = self.inner().scope.insert(name.clone(), val.into()) else {
            panic!();
        };
    }
    pub fn find(&mut self, name: &str) -> Option<eval::Value> {
        if Some(name) == self.refs.get(name).map(|v| v.as_ref()) {
            return self.ref_inner.as_ref().unwrap().as_ref().borrow().scope.get(name).cloned()
        }
        let Some(foo) = self.inner().scope.get(name).cloned() else {
            return self.inner().parent.as_mut().map(|p| p.find(name)).unwrap_or(None);
        };
        return Some(foo);
    }
    pub fn set(&mut self, name: &str, val: Value) {
        if Some(name) == self.refs.get(name).map(|v| v.as_ref()) {
            self.ref_inner.as_ref().unwrap().borrow_mut().scope.get_mut(name).map(|v| *v = val);
            return;
        }
        match self.inner().scope.get_mut(name) {
            Some(v) => {
                *v = val;
                return;
            },
            None => (),
        };
        self.inner().parent.as_mut().map(|p| p.set(name, val)).unwrap();
    }
    pub fn push_scope(&mut self) {
        *self = Env {
            inner: Rc::new(RefCell::new(EnvInner {
                scope: HashMap::new(),
                parent: Some(self.clone()),
            })),
            n: self.n + 1,
            refs: HashSet::new(),
            ref_inner: None,
        };
        
    }
    pub fn pop_scope(&mut self) {
        let p = self.inner().parent.clone().unwrap();
        *self = p;
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
                let mut new_env = env.closure();
                let func = eval::Value::Func(args.clone(), body.clone(), new_env.clone());
                env.assign(name, func.clone());
                new_env.assign(name, func);
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
            Statement::StructDef(name, members) => {
                let sd = Value::StructDef(name.clone(), members.iter().map(|t| {
                    let crate::ast::Token::Symbol(n) = t else {
                        panic!();
                    };
                    n.clone()
                }).collect());
                env.assign(name, sd);
            }
            Statement::EnumDef(name, members) => {
                let ed = Value::EnumDef(name.clone(), members.iter().enumerate().map(|(i,t)| {
                    let crate::ast::Token::Symbol(n) = t else {
                        panic!();
                    };
                    env.assign(n, Value::EnumMember(name.clone(), i));
                    n.clone()
                }).collect()); 
                env.assign(name, ed);
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