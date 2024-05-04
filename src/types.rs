use crate::ast::Expr;
use crate::eval;
use std::collections::HashMap;
use std::mem;
use std::rc::Rc;
use std::sync::Arc;

enum Type {
    Int,
    String,
    Bool,
    Nothin,
    Func(Vec<Type>, Box<Type>),
    Struct()
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Type::Func(..), t) => false,
            (t1, t2) => {
                mem::discriminant(t1) == mem::discriminant(t2)
            }
        }
    }
}

struct EnvNode {
    parent: Rc<EnvNode>,
    env: HashMap<Arc<str>, Type>,
}

struct TypeEnv {
    inner: Rc<EnvNode>
}

pub fn eval_type(expr: &Expr, env: &mut TypeEnv) -> Result<Type,()> {
    match e {
        Expr::Assign(var,value) => {
            Ok(Type::Nothin)
        }
        Expr::FnCall(f, args) => {
            let Type::Func(args_t, ret) = eval_type(&f, env).unwrap() else {
                return Err(());
            };
            for (i,a) in args.iter().enumerate() {
                if args_t[i] != eval_type(&a, env).unwrap() {
                    return Err(());
                }
            }
            Ok(*ret)
        }
        Expr::Binary(lhs, op, rhs) => {

        }
        Expr::Get(val, field) => {

        }
    }
}