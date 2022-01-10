use enum_map::Enum;
use lazy_static::lazy_static;
use std::collections::HashMap;

lazy_static! {
    pub static ref ONE_CHAR_OPERATORS: HashMap<&'static str, Token> = HashMap::from([
        ("{", Token::LBrace),
        ("}", Token::RBrace),
        ("(", Token::LParen),
        (")", Token::RParen),
        ("[", Token::LBracket),
        ("]", Token::RBracket),
        ("+", Token::Plus),
        ("*", Token::Star),
        ("/", Token::Div),
        (":", Token::Colon),
        (";", Token::Semi),
        (",", Token::Coma),
        (".", Token::Dot),
        ("-", Token::Minus),
        ("=", Token::Assign),
        ("<", Token::Less),
        (">", Token::Greater),
    ]);
    pub static ref TWO_CHAR_OPERATORS: HashMap<&'static str, Token> = HashMap::from([
        ("->", Token::RArrow),
        ("==", Token::Equal),
        ("<=", Token::LessEqual),
        (">=", Token::GreaterEqual),
        ("!=", Token::NotEqual),
    ]);
    pub static ref KEYWORDS: HashMap<&'static str, Token> = HashMap::from([
        ("dflow", Token::Dflow),
        ("module", Token::Module),
        ("library", Token::Library),
        ("component", Token::Component),
        ("return", Token::Return),
        ("fn", Token::Fn),
        ("let", Token::Let),
        ("while", Token::While),
        ("for", Token::For),
        ("in", Token::In),
        ("if", Token::If),
        ("else", Token::Else),
        ("struct", Token::Struct),
        ("init", Token::Init),
        ("call", Token::Call),
        ("impl", Token::Impl),
        ("as", Token::As),
        ("pre", Token::Pre),
        ("post", Token::Post),
        ("break", Token::Break),
        ("continue", Token::Continue),
        ("exec", Token::Exec),
        ("bind", Token::Bind),
        ("mut", Token::Mut),
        ("or", Token::Or),
        ("and", Token::And),
        ("not", Token::Not),
        ("u32", Token::U32),
        ("i32", Token::I32),
        ("f32", Token::F32),
        ("u64", Token::U64),
        ("i64", Token::I64),
        ("f64", Token::F64),
        ("u8", Token::U8),
        ("vec", Token::Vec),
        ("bool", Token::Bool),
        ("str", Token::String),
        ("false", Token::False),
        ("true", Token::True),
    ]);
}

#[derive(Clone, PartialEq, Debug)]
pub enum Token {
    Dflow,
    Module,
    Library,
    Component,
    Return,
    Fn,
    Let,
    While,
    For,
    In,
    If,
    Else,
    Struct,
    Init,
    Call,
    Impl,
    As,
    Pre,
    Post,
    Break,
    Continue,
    Exec,
    Bind,
    Mut,
    Or,
    And,
    Not,
    U32,
    I32,
    F32,
    U64,
    I64,
    F64,
    U8,
    Vec,
    Bool,
    String,
    LBrace,
    RBrace,
    LParen,
    RParen,
    LBracket,
    RBracket,
    Plus,
    Minus,
    Star,
    Div,
    Colon,
    Semi,
    Coma,
    Dot,
    Assign,
    Less,
    Greater,
    LessEqual,
    GreaterEqual,
    Equal,
    NotEqual,
    RArrow,
    True,
    False,
    M,
    StringLiteral(String),
    IntegerLiteral(u64),
    FloatingLiteral(f64),
    Identifier(String),
    Space,
    Tab,
    Newline,
    Comment(String),
    Empty,
    EndOfText,
}
