#[macro_use]
extern crate enum_map;
use enum_map::EnumMap;
use lexer::{Lexer, Source, Token};
use parser::lr_parser::{SymbolSource};
use std::collections::HashSet;
use lazy_static::lazy_static;
use std::{collections::HashMap};
use parser::grammar::Symbol;

pub struct LexerWrapper<T: Source> {
    lexer: Lexer<T>,
}
impl<T: Source> LexerWrapper<T> {
    pub fn new(lexer: Lexer<T>) -> LexerWrapper<T> {
        LexerWrapper{ lexer }
    }
}
impl<T: Source> SymbolSource for LexerWrapper<T> {
    fn next_symbol(&mut self) -> Symbol { 
        let t = Token::Empty;
        loop {
            let t = self.lexer.next_token();
            match t {
                Token::Comment(_) => continue,
                Token::Tab | Token::Newline | Token::Space => continue,
                _ => break,
            }   
        }
        match t {
            Token::Dot => Symbol::new("Dot"),
            Token::LParen => Symbol::new("LParen"),
            Token::RParen => Symbol::new("RParen"),
            Token::LBracket => Symbol::new("LBracket"),
            Token::RBracket => Symbol::new("RBracket"),
            Token::Not => Symbol::new("Not"),
            Token::Plus => Symbol::new("Plus"),
            Token::Minus => Symbol::new("Minus"),
            Token::Star => Symbol::new("Star"),
            Token::Div => Symbol::new("Div"),
            Token::Less => Symbol::new("Less"),
            Token::Greater => Symbol::new("Greater"),
            Token::LessEqual => Symbol::new("LessEqual"),
            Token::GreaterEqual => Symbol::new("GreaterEqual"),
            Token::Equal => Symbol::new("Equal"),
            Token::True => Symbol::new("True"),
            Token::False => Symbol::new("False"),
            Token::NotEqual => Symbol::new("NotEqual"),
            Token::U32 => Symbol::new("U32"),
            Token::I32 => Symbol::new("I32"),
            Token::F32 => Symbol::new("F32"),
            Token::U64 => Symbol::new("U64"),
            Token::I64 => Symbol::new("I64"),
            Token::F64 => Symbol::new("F64"),
            Token::U8 => Symbol::new("U8"),
            Token::Bool => Symbol::new("Bool"),
            Token::String => Symbol::new("String"),
            Token::IntegerLiteral(i) => Symbol::new("IntegerLiteral"),
            Token::FloatingLiteral(f) => Symbol::new("FloatingLiteral"),
            Token::StringLiteral(s) => Symbol::new("StringLiteral"),
            Token::Identifier(s) => Symbol::new("Identifier"),
            t => panic!("I don't know of token {:?}", t),
        }
    }
}


// ("expr"),
// ("conjunctions"),
// ("conjunctions"),
// ("inversions"),
// ("inversion"),
// ("comparison"),
// ("sum"),
// ("term"),
// ("factor"),
// ("argSeq"),
// ("expr"),
// ("primary"),
// ("atom"),
// ("unaryOperator"),
// ("addOperator"),
// ("multOperator"),
// ("relOperator"),
// ("literal"),
// ("typeSpecifier"),
// ("simpleTypeSpecifier"),
// ("booleanLiteral")
