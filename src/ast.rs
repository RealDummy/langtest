use std::fmt::{Debug, Write};
use std::rc::Rc;
use std::{collections::HashMap, ops::Range};
use std::sync::{Arc, TryLockResult};
use lazy_static;

use crate::run;

#[derive(Clone)]
pub enum Token {
    Plus,
    Minus,
    Star,
    Slash,
    Equal,
    Bang,
    Colon,
    DoubleEqual,
    BangEqual,
    LT,
    GT,
    LTE,
    GTE,
    WhiteSpace,
    NewLine,
    LParen,
    RParen,
    LCurly,
    RCurly,
    LSquare,
    RSquare,
    True,
    False,
    Print,
    Let,
    If,
    Else,
    Number(Arc<str>),
    Symbol(Arc<str>),
    EscapedLiteral(Arc<str>),
    EOF,
    Comma,
    Func,
    Return,
    While,
}
impl Debug for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Token::*;
        let mut cc = |s: &str| f.write_str(s);
        match self {
            Plus=>cc("+"),
            Minus=>cc("-"),
            Star=>cc("*"),
            Slash=>cc("/"),
            Equal=>cc("="),
            Bang=>cc("!"),
            Colon=>cc(":"),
            DoubleEqual=>cc("=="),
            BangEqual=>cc("="),
            LT=>cc("<"),
            GT=>cc(">"),
            LTE=>cc("<="),
            GTE=>cc(">="),
            WhiteSpace=>cc(" "),
            LParen=>cc("("),
            RParen=>cc(")"),
            LCurly=>cc("{"),
            RCurly=>cc("}"),
            LSquare=>cc("["),
            RSquare=>cc("]"),
            NewLine=>cc("\\n"),
            True=>cc("-true-"),
            False=>cc("-false-"),
            Print=>cc("-print-"),
            Let=>cc("-let-"),
            Number(s)=>cc(s),
            Symbol(s)=>cc(s),
            EscapedLiteral(s)=>{
                cc("\"")?;
                cc(s)?;
                cc("\"")?;
                Ok(())
            }
            EOF => Ok(()),
            If => cc("if"),
            Else => cc("else"),
            Comma => cc(","),
            Func => cc("func"),
            Return => cc("return"),
            While => cc("while"),
        }
    }
}

impl PartialEq for Token {
    fn eq(&self, other: &Self) -> bool {
        use Token::*;
        match (self, other) {
            (&Number(_), &Number(_)) => true,
            (&Symbol(_), &Symbol(_)) => true,
            (&EscapedLiteral(_), &EscapedLiteral(_)) => true,
            (t1, t2) if std::mem::discriminant(t1) == std::mem::discriminant(t2)  => true,
            _ => false
        }
    }
}

type Boxpr = Box<Expr>;

#[derive(PartialEq, Clone)]
pub enum Expr {
    Binary(Boxpr, Token, Boxpr),
    LUnary(Token, Boxpr),
    RUnary(Boxpr, Token),
    Literal(Token),
    Assign(Token, Boxpr),
    Group(Boxpr),
    FnCall(Boxpr, Vec<Expr>),
}

impl Debug for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Binary(lhs, op, rhs) => f.write_fmt(format_args!("<{lhs:?} {op:?} {rhs:?}>")),
            Expr::LUnary(op, e) => f.write_fmt(format_args!("{:?}{:?}", op, e)),
            Expr::RUnary(..)=>todo!(),
            Expr::Literal(t) => f.write_fmt(format_args!("{:?}", t)),
            Expr::Group(g)=>f.write_fmt(format_args!("( {g:?} )")),
            Expr::Assign(n, v)=>f.write_fmt(format_args!("{:?}={:?}", n,v)),
            Expr::FnCall(fe, args) => {
                f.write_fmt(format_args!("{:?}( ", fe))?;
                for (i, arg) in args.iter().enumerate() {
                    match i {
                        n if n == args.len() => f.write_fmt(format_args!("{:?} )", arg)),
                        _ => f.write_fmt(format_args!("{:?}, ", arg)),
                    };
                };
                Ok(())
            } ,

        }
    }
}

#[derive(Debug)]
pub struct ParserError {
    pub line: usize,
    pub char: usize,
    pub reason: String,
}

#[derive(Debug)]
enum ParserState {
    Error(ParserError),
    Ok,
}

#[derive(Debug)]
pub struct Tokenized {
    tokens: Vec<Token>,
    current: usize,
    prev_token: Option<usize>,
    line_count: usize,
    state: ParserState
}
impl Tokenized {
    pub fn new(input: &str) -> Self {
        let mut curr_token = 0..1;
        let mut tokens = Vec::new();
        while curr_token.end <= input.len()  {
            match extract_token(curr_token.clone(), input) {
                Some(t) => {
                    tokens.push(t.0);
                    curr_token = t.1..t.1 + 1;
                }
                None => {
                    curr_token = curr_token.start..curr_token.end + 1;
                }
            }
        }
        if let Some(t) = extract_token(curr_token.clone(), input) {
            tokens.push(t.0);
        }

        Self {
            tokens,
            current: 0,
            prev_token: None,
            line_count: 0,
            state: ParserState::Ok
        }
    }
    fn prev(&self) -> Option<Token> {
        self.tokens.get(self.prev_token?).cloned()
    }
    fn skip_blank_space(&mut self) {
        loop{
            match self.peek() {
                Some(Token::WhiteSpace) => (),
                Some(Token::NewLine) => {break;},
                _ => {break;}
            }
            self.current += 1;
        }
    }
    fn skip_space(&mut self) {
        loop{
            match self.peek() {
                Some(Token::WhiteSpace) => (),
                Some(Token::NewLine) => {self.line_count += 1;},
                _ => {break;}
            }
            self.current += 1;
        }
    }

    fn skip_next(&mut self) {
        self.current += 1;
        self.skip_blank_space();
    }
    pub fn advance(&mut self) -> Option<Token> {
        self.prev_token = Some(self.current);
        self.skip_next();
        if self.current > self.tokens.len() {
            return None;
        }
        self.prev()
    }
    pub fn peek(&self) -> Option<Token> {
        self.tokens.get(self.current).cloned()
    }
    pub fn matches(&mut self, ops: &[Token]) -> Option<bool> {
        let Some(op) = self.peek() else {
            return None;
        };
        if ops.iter().any(|t| op == *t) {
            self.advance();
            return Some(true);
        }
        Some(false)
    }
    pub fn consume(&mut self, expected: Token, err: &str) -> Token {
        if Some(expected) == self.peek() {
            return self.advance().unwrap();
        }
        panic!("parse error: {err}\n({}): {:?}", self.line_count, &self.tokens[self.current..(self.current + 4).min(self.tokens.len())]);
    }
}

lazy_static::lazy_static! {
    static ref TOKEN_MAP: HashMap<&'static str, Token> = {
        use Token::*;
        HashMap::from([
            ("+", Plus),
            ("-", Minus),
            ("*", Star),
            ("/", Slash),
            ("=", Equal),
            ("!", Bang),
            (":", Colon),
            ("==", DoubleEqual),
            ("!=", DoubleEqual),
            ("<", LT),
            (">", GT),
            ("<=", LTE),
            (">=", GTE),
            ("(", LParen),
            (")", RParen),
            ("{", LCurly),
            ("}", RCurly),
            ("[", LSquare),
            ("]", RSquare),
            ("true", True),
            ("false", False),
            ("print", Print),
            ("let", Let),
            ("\n", NewLine),
            ("\r\n", NewLine),
            ("if", If),
            ("else", Else),
            (",", Comma),
            ("func", Func),
            ("return", Return),
            ("while", While),
        ])
    };
    static ref DUMB_ESCAPED_LITERAL: Token = Token::EscapedLiteral("".into());
    static ref DUMB_NUMBER: Token = Token::Number("".into());
    static ref DUMB_SYMBOL: Token = Token::Symbol("".into());
    static ref LITERAL_TOKENS: [Token; 6] = [DUMB_ESCAPED_LITERAL.clone(), DUMB_NUMBER.clone(), DUMB_SYMBOL.clone(), Token::True, Token::False, Token::Print];
}
const EQUALITY_OP: &[Token] = {
    use Token::*;
    &[DoubleEqual, BangEqual]
};
const COMPARE_OP: &[Token] = {
    use Token::*;
    &[LT, GT, LTE, GTE]
};
const SHIFT_OP: &[Token] = {
    use Token::*;
    &[Plus, Minus]
};
const SCALE_OP: &[Token] = {
    use  Token::*;
    &[Star, Slash]
};
const LEFT_UNARY_OP: &[Token] = {
    use  Token::*;
    &[Star, Slash]
};

#[derive(Debug, Clone)]
pub enum Statement {
    Expression(Expr),
    Block(Vec<Statement>),
    Print(Expr),
    Conditional(Expr, Box<Statement>, Option<Box<Statement>>),
    Var(Arc<str>, Expr),
    FuncDef(Arc<str>, Vec<Token>, Box<Statement>),
    Return(Option<Expr>),
    While(Expr, Box<Statement>),
}
impl PartialEq for Statement {
    fn eq(&self, other: &Self) -> bool {
        false
    }
}

pub fn parse(tokens: &mut Tokenized) -> Vec<Statement> {
    let mut prog = Vec::new();
    tokens.skip_space();
    while tokens.peek() != Some(Token::EOF) {
        let stmt = declaration(tokens);
        prog.push(stmt);
        tokens.skip_space();
    }
    prog

}

pub fn declaration(tokens: &mut Tokenized) -> Statement {

    if let Some(true) = tokens.matches(&[Token::Let]) {
        return var_decl(tokens);
    }
    if let Some(true) = tokens.matches(&[Token::Func]) {
        return func_decl(tokens);
    }
    return statement(tokens);
}
pub fn func_decl(tokens: &mut Tokenized) -> Statement {
    let Token::Symbol(name) = tokens.consume(DUMB_SYMBOL.clone(), "expected func name") else {
        panic!("heckin");
    };
    tokens.consume(Token::LParen, "func start with '('");
    let mut params = Vec::new();
    if Some(Token::RParen) != tokens.peek() {
        loop {
            params.push(tokens.consume(DUMB_SYMBOL.clone(), "expected arg"));
            
            let Some(true) = tokens.matches(&[Token::Comma]) else {
                break;
            };

        }
    }
    tokens.consume(Token::RParen, "expected ')' after func def");
    tokens.skip_space();
    tokens.consume(Token::LCurly, "expected '{' after func def");
    tokens.skip_space();
    let body = block(tokens);
    tokens.skip_space();
    return Statement::FuncDef(name, params, Box::new(body));

}
pub fn var_decl(tokens: &mut Tokenized) -> Statement {
    let Token::Symbol(name) = tokens.consume(DUMB_SYMBOL.clone(), "expected identifier") else {
        panic!();
    };
    tokens.consume(Token::Equal, "expected =");

    let init = expression(tokens);
    tokens.consume(Token::NewLine, "expected new line");

    return Statement::Var(name, init);
}

pub fn statement(tokens: &mut Tokenized) -> Statement {
    if tokens.matches(&[Token::Print]).unwrap() {
        return print_statement(tokens);
    }
    if tokens.matches(&[Token::Return]).unwrap() {
        return return_statement(tokens);
    }
    if tokens.matches(&[Token::LCurly]).unwrap() {
        return block(tokens);
    }
    if tokens.matches(&[Token::If]).unwrap() {
        return conditional(tokens);
    }
    if tokens.matches(&[Token::While]).unwrap() {
        return while_loop(tokens);
    }
    return expression_statement(tokens);
}
fn while_loop(tokens: &mut Tokenized) -> Statement {
    let condition = expression(tokens);
    tokens.skip_space();
    tokens.consume(Token::LCurly, "expected '{'");
    let stmt = block(tokens);
    return Statement::While(condition, Box::new(stmt));
}
fn conditional(tokens: &mut Tokenized) -> Statement {
    let condition = expression(tokens);
    tokens.skip_space();
    tokens.consume(Token::LCurly, "expected '{'");
    let stmt = block(tokens);
    let mut else_stmt = None;
    if tokens.matches(&[Token::Else]).unwrap_or(false) {
        tokens.skip_space();
        tokens.consume(Token::LCurly, "expected '{'");
        else_stmt = Some(block(tokens));
    }
    return Statement::Conditional(condition, Box::new(stmt), else_stmt.map(|e| {Box::new(e)}));

}

fn block(tokens: &mut Tokenized) -> Statement {
    let mut res = Vec::new();
    tokens.skip_space();
    while !tokens.matches(&[Token::RCurly]).unwrap_or(false) {
        res.push(declaration(tokens));
        tokens.skip_space();
    }
    tokens.skip_space();
    return Statement::Block(res);
}

pub fn print_statement(tokens: &mut Tokenized) -> Statement {
    let expr = expression(tokens);
    // tokens.skip_space();
    return Statement::Print(expr);
}
fn return_statement(tokens: &mut Tokenized) -> Statement {
    tokens.skip_space();
    match tokens.peek() {
        Some(Token::NewLine) | Some(Token::RCurly) | Some(Token::RParen) => {
            return Statement::Return(None);
        }
        _ => ()
    }
    let expr = expression(tokens);
    tokens.skip_space();
    return Statement::Return(Some(expr));
}
pub fn expression_statement(tokens: &mut Tokenized) -> Statement {
    let expr = expression(tokens);
    // tokens.skip_space();
    // tokens.consume(Token::NewLine, &format!("expected newline, got '{:?}'", tokens.peek()));
    // tokens.eat_space();
    return Statement::Expression(expr);
}

pub fn expression(tokens: &mut Tokenized) -> Expr {
    return assignment(tokens);
}

fn assignment(tokens: &mut Tokenized) -> Expr {

    let e = equality(tokens);
    if let Some(true) = tokens.matches(&[Token::Equal]) {
        let value = assignment(tokens);
        let Expr::Literal(name) = e else {
            panic!("expected var name");
        };
        return Expr::Assign(name, Box::new(value));
    }
    return e;
}

fn binop_impl(tokens: &mut Tokenized, ops: &[Token], lower_precedence: impl Fn(&mut Tokenized)-> Expr) -> Expr {
    let mut lhs = lower_precedence(tokens);
    while tokens.matches(ops).unwrap() {
        let op = tokens.prev().unwrap();
        let rhs = {
             lower_precedence(tokens)
        };
        lhs = Expr::Binary(Box::new(lhs), op, Box::new(rhs));
    }
    return lhs;
}

fn equality(tokens: &mut Tokenized) -> Expr {
    binop_impl(tokens, EQUALITY_OP, comparison)
}

fn comparison(tokens: &mut Tokenized) -> Expr {
    binop_impl(tokens, COMPARE_OP, shift)
}
fn shift(tokens: &mut Tokenized) -> Expr {
    binop_impl(tokens, SHIFT_OP, scale)
}
fn scale(tokens: &mut Tokenized) -> Expr {
    binop_impl(tokens, SCALE_OP, left_unary)
}
fn left_unary(tokens: &mut Tokenized) -> Expr {
    if let Some(true) = tokens.matches(LEFT_UNARY_OP) {
        let op = tokens.prev().unwrap();
        let expr = left_unary(tokens);
        return Expr::LUnary(op, Box::new(expr));
    }

    return call(tokens);
}
fn call(tokens: &mut Tokenized) -> Expr {
    let mut expr = primary(tokens);
    loop {
        if tokens.matches(&[Token::LParen]).unwrap_or(false) {
            expr = finish_call(tokens, expr);
        } else {
            break;
        }
    }
    return expr;
}
fn finish_call(tokens: &mut Tokenized, callee: Expr) -> Expr {
    let mut args = Vec::new();
        if let Some(Token::RParen) = tokens.peek() {
            tokens.advance();
            return Expr::FnCall(Box::new(callee), args);
        }
        loop {
            args.push(expression(tokens));
            let Some(Token::Comma) = tokens.peek() else {
                break;
            };
            tokens.advance();
        }
        tokens.consume(Token::RParen, "expected ')'");
        return Expr::FnCall(Box::new(callee), args);
}
fn primary(tokens: &mut Tokenized) -> Expr {
    if let Some(true) = tokens.matches(&(*LITERAL_TOKENS))  {
        return Expr::Literal(tokens.prev().unwrap());
    }
    if let Some(true) = tokens.matches(&[Token::LParen]) {
        let exp = expression(tokens);
        tokens.consume(Token::RParen, "expected ')'.");
        return Expr::Group(Box::new(exp));
    }
    match tokens.peek() {
        n@Some(Token::NewLine) => {
            // tokens.advance();
            tokens.skip_space();
            println!("{n:?} {:?}", &tokens.tokens[tokens.current..tokens.current+2]);
            return expression(tokens);
        },
        _ => ()
    }
    panic!("syntax error {:?} ({:?})", tokens.peek(), tokens);
}

fn extract_stringy_token(slice: Range<usize>, input: &str) -> Option<Token> {
    if input[slice.clone()].chars().next() == Some('\"') { 
        if input[slice.clone()].chars().last() == Some('\"') && slice.len() != 1 {
            return Some(Token::EscapedLiteral(Arc::from(&input[slice.clone()])));
        } else {
            return None;
        }
    };
    if input[slice.clone()].chars().all(|c| c.is_alphanumeric() || c == '_') {
        return Some(Token::Symbol(Arc::from(&input[slice.clone()])));
    }
    return None;
}

fn is_operator(t: &Token) -> bool {
    use Token::*;
    matches!(t, Plus | Minus |Star | Slash| Equal | DoubleEqual | LT | GT | LTE | GTE )
}

fn extract_token(slice: Range<usize>, input: &str) -> Option<(Token, usize)> {
    if slice.end > input.len() {
        return Some((Token::EOF, slice.end));
    }
    match TOKEN_MAP.get(&input[slice.clone()]) {
        Some(t) => {
            match extract_token(slice.start..slice.end + 1, input) {
                None => Some((t.clone(), slice.end)),
                Some(t2) => if is_operator(&t2.0) {
                    Some(t2)
                }
                else {
                    Some((t.clone(), slice.end))
                }
            }
        }
        None => {
            if input[slice.clone()].chars().all(|c| c.is_ascii_whitespace()) {
                return Some((Token::WhiteSpace, slice.end));
            }
            if input[slice.clone()].chars().all(|c| c.is_digit(10)) {
                match extract_token(slice.start..slice.end + 1, input) {
                    None => {
                        return Some((Token::Number(input[slice.clone()].into()), slice.end));
                    }
                    Some(n@(Token::Number(_), _)) => {
                        return Some(n);
                    }
                    _ => {
                        return Some((Token::Number(input[slice.clone()].into()), slice.end))
                    }
                }
            };
            match extract_token(slice.end..slice.end+1, input) {
                Some((t, _)) => {
                    if matches!(t, Token::Symbol(_) | Token::EscapedLiteral(_) | Token::Number(_)) {
                        ();
                     } else{
                        return extract_stringy_token(slice.clone(), input).map(|t| (t, slice.end))
                    }
                },
                None => return extract_stringy_token(slice.clone(), input).map(|t| (t, slice.end)),
            }
            match extract_token(slice.start..slice.end + 1, input) {
                None => return extract_stringy_token(slice.clone(), input).map(|t| (t, slice.end)),
                t => {
                    return t;
                }
            };
        }
    }
}


#[ignore = "helper"]
fn var(name: &str) -> Boxpr {
    Box::new(Expr::Literal(Token::Symbol(name.into())))
}

#[test]
fn add() {
    let input = r#"a+b"#;
    let output = Expr::Binary(var("a"), Token::Plus, var("b"));
    assert_eq!(expression(&mut Tokenized::new(input)), output);
}
#[test]
fn precedence() {
    let input = r#"a+b*c"#;
    let output = Expr::Binary(var("a"), Token::Plus, Box::new(Expr::Binary(var("b"), Token::Star, var("c"))));
    assert_eq!(expression(&mut Tokenized::new(input)), output);
}

#[test]
fn stringy() {
    let input = r#"" 1 +2" + "3""#;
    let output = Expr::Binary(Box::new(Expr::Literal(Token::EscapedLiteral("\" 1 +2\"".into()))), Token::Plus, Box::new(Expr::Literal(Token::EscapedLiteral("\"3\"".into()))));
    assert_eq!(expression(&mut Tokenized::new(input)), output);
}

#[test]
fn group() {
    let input = r#"(a+b)*c"#;
    let output = Expr::Binary(Box::new(Expr::Group(Box::new(Expr::Binary(var("a"), Token::Plus, var("b"))))), Token::Star, var("c"));
    assert_eq!(expression(&mut Tokenized::new(input)), output);
}

#[test]
fn white() {
    let input = r#"aaaa    +      bb ==  
ccccccccc
    "#;
    let out = Expr::Binary(Box::new(Expr::Binary(var("aaaa"), Token::Plus, var("bb"))), Token::DoubleEqual, var("ccccccccc"));
    assert_eq!(expression(&mut Tokenized::new(input)), out);
    let input = r#""foo" "#;
    let out = Expr::Literal(Token::EscapedLiteral(r#""foo""#.into()));
    assert_eq!(expression(&mut Tokenized::new(input)), out);

}

#[test]
fn bool() {
    let input = "false";
    let output = Expr::Literal(Token::False);
    assert_eq!(expression(&mut Tokenized::new(input)), output);
}

#[test]
fn assign() {
    let input = "a=b";
    let out = Expr::Assign(Token::Symbol("a".into()), var("b"));
    assert_eq!(expression(&mut Tokenized::new(input)), out);
}
#[test]
fn block_spacing() {
    let input = "if 1==2{}";
    parse(&mut Tokenized::new(input));
}