
use std::{cell::RefCell, collections::{HashMap, HashSet}, fmt::Debug, rc::Rc, sync::Arc};

use crate::ast::Statement;

type TypeId = u32;

pub enum ConcreteTypes {
    Int,
    String,
    Bool,
    Func(usize),
    EnumMember(usize),
    Struct(usize),
}
impl ConcreteTypes {
    pub fn type_id(&self) -> TypeId {
        use ConcreteTypes::*;
        match self {
            Int => 1,
            String => 2,
            Bool => 3,
            _ => todo!(), 
        }
    }
}


#[derive(Clone, Debug)]
pub enum Ty {
    Explicit,
    

}

#[derive(Clone, Debug)]
pub struct EnvInner {
    parent: Option<TypeEnv>,
    scope: HashMap<Arc<str>, Ty>,
}
impl PartialEq for EnvInner {
    fn eq(&self, _other: &Self) -> bool {
        false
    }
}

#[derive(Clone, PartialEq)]
pub struct TypeEnv {
    inner: Rc<RefCell<EnvInner>>,
    refs: HashSet<Arc<str>>,
    ref_inner: Option<Rc<RefCell<EnvInner>>>,
    n: usize,
}

impl Debug for TypeEnv {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}", self.n))
    }
}

impl TypeEnv {
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
        let mut shallow_env = TypeEnv::new();
        shallow_env.inner().parent = parent_env;
        shallow_env.refs.extend(self.inner().scope.keys().cloned());
        shallow_env.ref_inner = Some(self.inner.clone());
        return shallow_env;
    }
    fn inner<'a>(&'a mut self)-> std::cell::RefMut<'a, EnvInner> {
        self.inner.as_ref().borrow_mut()
    }
    pub fn assign(&mut self, name: &Arc<str>, val: Ty) {
        let None = self.inner().scope.insert(name.clone(), val.into()) else {
            panic!();
        };
    }
    pub fn find(&mut self, name: &str) -> Option<Ty> {
        if Some(name) == self.refs.get(name).map(|v| v.as_ref()) {
            return self.ref_inner.as_ref().unwrap().as_ref().borrow().scope.get(name).cloned()
        }
        let Some(foo) = self.inner().scope.get(name).cloned() else {
            return self.inner().parent.as_mut().map(|p| p.find(name)).unwrap_or(None);
        };
        return Some(foo);
    }
    pub fn set(&mut self, name: &str, val: Ty) {
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
        *self = TypeEnv {
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

pub struct TypeChecker {
    prog: Vec<Statement>,
}
impl TypeChecker {
    pub fn new(prog: Vec<Statement>) -> Self {
        Self {
            prog,
        }
    }
    pub fn check(&mut self) {
        
    }
}