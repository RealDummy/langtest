use std::fmt::{Binary, Debug, Write};
use std::rc::Rc;
use std::{collections::HashMap, ops::Range};
use std::sync::{Arc, Once};
use lazy_static;

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
    Number(Arc<str>),
    Symbol(Arc<str>),
    EscapedLiteral(Arc<str>),
    EOF,
}
impl Debug for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Token::*;
        let mut cc = |s: &str| s.chars().for_each(|c| {f.write_char(c);});
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
                cc("\"");
                cc(s);
                cc("\"");
            }
            EOF => (),
        };
        Ok(())
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

#[derive(PartialEq)]
pub enum Expr {
    Binary(Boxpr, Token, Boxpr),
    LUnary(Token, Boxpr),
    RUnary(Boxpr, Token),
    Literal(Token),
    Group(Boxpr),
}

impl Debug for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Binary(lhs, op, rhs) => f.write_fmt(format_args!("<{lhs:?} {op:?} {rhs:?}>")),
            Expr::LUnary(op, e) => f.write_fmt(format_args!("{:?}{:?}", op, e)),
            Expr::RUnary(..)=>todo!(),
            Expr::Literal(t) => f.write_fmt(format_args!("{:?}", t)),
            Expr::Group(g)=>f.write_fmt(format_args!("( {g:?} )")),
        }
    }
}

#[derive(Debug)]
pub struct Tokenized {
    tokens: Vec<Token>,
    current: usize,
    prev_token: Option<usize>,
    line_count: usize,
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
        }
    }
    fn prev(&self) -> Option<Token> {
        self.tokens.get(self.prev_token?).cloned()
    }
    fn skip_whitespace(&mut self) {
        loop{
            match self.peek() {
                Some(Token::WhiteSpace) => (),
                Some(Token::NewLine) => {self.line_count += 1; break;},
                _ => {break;}
            }
            self.current += 1;
        }
    }
    fn skip_next(&mut self) {
        self.current += 1;
        self.skip_whitespace();
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
        panic!("parse error: {err}");
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

#[derive(Debug)]
pub enum Statement {
    Expression(Expr),
    Print(Expr),
    Var(Arc<str>, Expr),
}

pub fn parse(tokens: &mut Tokenized) -> Vec<Statement> {
    let mut prog = Vec::new();
    while tokens.peek() != Some(Token::EOF) {
        prog.push(declaration(tokens))
    }
    prog

}

pub fn declaration(tokens: &mut Tokenized) -> Statement {

    if let Some(true) = tokens.matches(&[Token::Let]) {
        return var_decl(tokens);
    }
    return statement(tokens);
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
    return expression_statement(tokens);
}

pub fn print_statement(tokens: &mut Tokenized) -> Statement {
    let expr = expression(tokens);
    tokens.consume(Token::NewLine, "expected newline");
    return Statement::Print(expr);
}
pub fn expression_statement(tokens: &mut Tokenized) -> Statement {
    let expr = expression(tokens);
    tokens.consume(Token::NewLine, "expected newline");
    return Statement::Expression(expr);
}

pub fn expression(tokens: &mut Tokenized) -> Expr {
    return equality(tokens);
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
    primary(tokens)
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
    if let Some(true) = tokens.matches(&[Token::NewLine]) {
        return expression(tokens);

    }
    panic!("syntax error {:?}", tokens);
}

fn extract_stringy_token(slice: Range<usize>, input: &str) -> Option<Token> {
    if input[slice.clone()].chars().next() == Some('\"') { 
        if input[slice.clone()].chars().last() == Some('\"') && slice.len() != 1 {
            return Some(Token::EscapedLiteral(Arc::from(&input[slice.clone()])));
        } else {
            return None;
        }
    };
    return Some(Token::Symbol(Arc::from(&input[slice.clone()])));
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
                }else {
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
            }
            match extract_token(slice.end..slice.end+1, input) {
                Some((t, _)) => {
                    if matches!(t, Token::Symbol(_) | Token::EscapedLiteral(_)) {
                        None
                     }else{
                        extract_stringy_token(slice.clone(), input).map(|t| (t, slice.end))
                    }
                },
                None => None,
            }
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