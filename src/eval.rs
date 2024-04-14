use std::{fmt::Display, sync::Arc};

use crate::{ast::{Expr, Statement, Token}, run::{eval_statement, Env}};

type StringType = String;
type IntType = i64;

#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    String(StringType),
    Int(IntType),
    Bool(bool),
    Func(Vec<Token>, Box<Statement>, Env),
    Unit,
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Bool(b) => f.write_fmt(format_args!("{}", b)),
            Value::Int(i) => f.write_fmt(format_args!("{}", i)),
            Value::String(s) => f.write_str(&s),
            Value::Unit => f.write_str("()"),
            Value::Func(..) => todo!(),
        }
    }
}

fn eq<T: PartialEq>(l:T, r:T) -> Value {
    match l == r {
        true => Value::Bool(true),
        false => Value::Bool(false),
    }
}

fn neq<T: PartialEq>(l:T, r:T) -> Value {
    match l != r {
        true => Value::Bool(true),
        false => Value::Bool(false),
    }
}

fn cmp_lt<T: Ord>(l:T, r:T) -> Value {
    match l < r {
        true => Value::Bool(true),
        false => Value::Bool(false),
    }
}
fn cmp_lte<T: Ord>(l:T, r:T) -> Value {
    match l <= r {
        true => Value::Bool(true),
        false => Value::Bool(false),
    }
}

fn apply_str(mut l: StringType, op: &Token, r: StringType) -> Value {
    match op {
        Token::Plus => {
            l.extend(r.chars());
            Value::String(l)
        }
        Token::DoubleEqual => {
            eq(l,r)
        }
        Token::BangEqual => {
            neq(l, r)
        }
        _ => {
            panic!("cant apply {op:?} to strings")
        }
    }
}

fn apply_int(l: IntType, op: &Token, r: IntType) -> Value {
    let res = match op {
        Token::Plus => l+r,
        Token::Minus =>l-r,
        Token::Star => l*r,
        Token::Slash =>l/r,
        Token::DoubleEqual => {
            return eq(l, r)
        }
        Token::BangEqual => {
            return neq(l,r)
        }
        Token::LT => {
            return cmp_lt(l, r);
        }
        Token::LTE => {
            return cmp_lte(l, r);
        }
        Token::GT => {
            return cmp_lte(r, l);
        }
        Token::GTE => {
            return cmp_lt(r, l);
        }
        _ => {
            panic!("unexpected {op:?}");
        }

    };
    return Value::Int(res);

}
fn apply_bool(l: bool, op: &Token, r: bool) -> Value {
    match op {
        Token::DoubleEqual => {
            return eq(l, r)
        }
        Token::BangEqual => {
            return neq(l,r)
        }
        _ => {
            panic!("unexpected {op:?}");
        }
    }
}

fn eval_bin(l: Value, op: &Token, r: Value) -> Value {
    match l {
        Value::String(ls) => {
            let Value::String(rs) = r else {
                panic!();
            };
            return apply_str(ls, op,  rs);
        }
        Value::Int(li) => {
            let Value::Int(ri) = r else {
                panic!();
            }; 
            return apply_int(li, op, ri);
        }
        Value::Bool(lb) => {
            let Value::Bool(rb) = r else {
                panic!();
            }; 
            return apply_bool(lb, op, rb);
        }
        Value::Unit => panic!(),
        Value::Func(..) => panic!(),
    }
}

fn eval_l_unary(op: &Token, e: Value) -> Value {
    todo!()
}

fn unescape_string(escaped: &str) -> &str {
    &escaped[1..escaped.len()-1]
}

pub fn eval(e: &Expr, env: &mut Env) -> Value {
    match e {
        Expr::Binary(lhs, op, rhs) => {
            eval_bin(eval(lhs, env), op, eval(rhs, env))
        }
        Expr::LUnary(op, e) => {
            eval_l_unary(op, eval(e, env))
        }
        Expr::Literal(t) => {
            match t {
                Token::EscapedLiteral(l) => {
                    Value::String(unescape_string(l).to_owned())
                }
                Token::Number(n) => {
                    Value::Int(n.parse().unwrap())
                }
                Token::Symbol(s) => {
                    env.find(s).unwrap()
                }
                Token::True => Value::Bool(true),
                Token::False => Value::Bool(false),
                _ => {
                    panic!()
                }
            }
        }
        Expr::FnCall(func, args) => {
            let Value::Func(arg_names, body, mut fn_env) = eval(func, env) else {
                panic!("expected func");
            };
            let args: Vec<_> = args.iter().map(|a| eval(a, env)).collect();
            assert!(args.len() == arg_names.len());
            for (name, val) in arg_names.iter().zip(args) {
                let Token::Symbol(name) = name else {
                    panic!("no symbol?");
                };
                env.assign(name, val);
            }
            let v = eval_statement(&body, &mut fn_env).unwrap_or(Value::Unit);
            v
        },
        Expr::Group(e) => eval(e, env),
        Expr::RUnary(.. ) => todo!(),
        Expr::Assign(name, value) => {
            let Token::Symbol(name) = name else {
                panic!("expected symbol")
            };
            let e = eval(value, env);
            env.assign(name, e);
            Value::Unit
        }
    }
}

// #[test]
// fn add() {
//     let input = r#"1+2"#;
//     let e = ast::expression(&mut ast::Tokenized::new(input));
//     assert!(eval(&e) == Value::Int(3))    
// }

// #[test]
// fn concat() {
//     let input = r#""foo" + "bar"+"biz""#;
//     let e = ast::expression(&mut ast::Tokenized::new(input));
//     assert!(eval(&e) == Value::String("foobarbiz".into()))    
// }

// #[test]
// fn super_math() {
//     let input = r#"1 + 2 * 3 + 4"#;
//     let e = ast::expression(&mut ast::Tokenized::new(input));
//     assert!(eval(&e) == Value::Int(11))    
// }