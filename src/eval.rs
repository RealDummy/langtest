use crate::ast::{Expr, self, Token};

#[derive(Debug, PartialEq)]
pub enum Value {
    String(String),
    Int(i32),
    Bool(bool)
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

fn apply_str(mut l: String, op: &Token, r: String) -> Value {
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

fn apply_int(mut l: i32, op: &Token, r: i32) -> Value {
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
    }
}

fn eval_l_unary(op: &Token, e: Value) -> Value {
    todo!()
}

pub fn eval(e: &Expr) -> Value {
    match e {
        Expr::Binary(lhs, op, rhs) => {
            eval_bin(eval(lhs), op, eval(rhs))
        }
        Expr::LUnary(op, e) => {
            eval_l_unary(op, eval(e))
        }
        Expr::Literal(t) => {
            match t {
                Token::EscapedLiteral(l) => {
                    Value::String(l[1..l.len()-1].to_owned())
                }
                Token::Number(n) => {
                    Value::Int(n.parse().unwrap())
                }
                Token::Symbol(s) => {
                    println!("{s}");
                    todo!()
                }
                _ => panic!()
            }
        }
        Expr::Group(e) => eval(e),
        Expr::RUnary(.. ) => todo!()
    }
}

#[test]
fn add() {
    let input = r#"1+2"#;
    let e = ast::expression(&mut ast::Tokenized::new(input));
    assert!(eval(&e) == Value::Int(3))    
}

#[test]
fn concat() {
    let input = r#""foo" + "bar"+"biz""#;
    let e = ast::expression(&mut ast::Tokenized::new(input));
    assert!(eval(&e) == Value::String("foobarbiz".into()))    
}

#[test]
fn super_math() {
    let input = r#"1 + 2 * 3 + 4"#;
    let e = ast::expression(&mut ast::Tokenized::new(input));
    assert!(eval(&e) == Value::Int(11))    
}