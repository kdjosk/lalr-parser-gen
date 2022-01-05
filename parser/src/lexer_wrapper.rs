use lexer::{Lexer, Source, Token};
use crate::lr_parser::{SymbolSource};
use std::collections::HashSet;
use lazy_static::lazy_static;
use std::{collections::HashMap};
use crate::grammar::Symbol;

pub struct LexerWrapper<T: Source> {
    lexer: Lexer<T>,
}
impl<T: Source> LexerWrapper<T> {
    pub fn new(lexer: Lexer<T>) -> LexerWrapper<T> {
        LexerWrapper{ lexer }
    }
}
impl<T: Source> SymbolSource for LexerWrapper<T> {
    fn next_symbol(&mut self) -> (Symbol, Token)  { 
        let mut t = Token::Empty;
        loop {
            t = self.lexer.next_token();
            println!("Got token {:?}", t);
            match t {
                Token::Comment(_) => continue,
                Token::Tab | Token::Newline | Token::Space => continue,
                _ => break,
            }   
        }

        match t {
            Token::Assign => (Symbol::new("Assign"), Token::Assign),
            Token::LBrace => (Symbol::new("LBrace"), Token::LBrace),
            Token::RBrace => (Symbol::new("RBrace"), Token::RBrace),
            Token::If => (Symbol::new("If"), Token::If),
            Token::Coma => (Symbol::new("Coma"), Token::Coma),
            Token::Semi => (Symbol::new("Semi"), Token::Semi),
            Token::Dot => (Symbol::new("Dot"), Token::Dot),
            Token::LParen => (Symbol::new("LParen"), Token::LParen),
            Token::RParen => (Symbol::new("RParen"), Token::RParen),
            Token::LBracket => (Symbol::new("LBracket"), Token::LBracket),
            Token::RBracket => (Symbol::new("RBracket"), Token::RBracket),
            Token::Not => (Symbol::new("Not"), Token::Not),
            Token::And => (Symbol::new("And"), Token::And),
            Token::Plus => (Symbol::new("Plus"), Token::Plus),
            Token::Minus => (Symbol::new("Minus"), Token::Minus),
            Token::Star => (Symbol::new("Star"), Token::Star),
            Token::Div => (Symbol::new("Div"), Token::Div),
            Token::Less => (Symbol::new("Less"), Token::Less),
            Token::Greater => (Symbol::new("Greater"), Token::Greater),
            Token::LessEqual => (Symbol::new("LessEqual"), Token::LessEqual),
            Token::GreaterEqual => (Symbol::new("GreaterEqual"), Token::GreaterEqual),
            Token::Equal => (Symbol::new("Equal"), Token::Equal),
            Token::True => (Symbol::new("True"), Token::True),
            Token::False => (Symbol::new("False"), Token::False),
            Token::NotEqual => (Symbol::new("NotEqual"), Token::NotEqual),
            Token::U32 => (Symbol::new("U32"), Token::U32),
            Token::I32 => (Symbol::new("I32"), Token::I32),
            Token::F32 => (Symbol::new("F32"), Token::F32),
            Token::U64 => (Symbol::new("U64"), Token::U64),
            Token::I64 => (Symbol::new("I64"), Token::I64),
            Token::F64 => (Symbol::new("F64"), Token::F64),
            Token::U8 => (Symbol::new("U8"), Token::U8),
            Token::Bool => (Symbol::new("Bool"), Token::Bool),
            Token::String => (Symbol::new("String"), Token::String),
            Token::IntegerLiteral(i) => (Symbol::new("IntegerLiteral"), Token::IntegerLiteral(i)),
            Token::FloatingLiteral(f) => (Symbol::new("FloatingLiteral"), Token::FloatingLiteral(f)),
            Token::StringLiteral(s) => (Symbol::new("StringLiteral"), Token::StringLiteral(s)),
            Token::Identifier(s) => (Symbol::new("Identifier"), Token::Identifier(s)),
            Token::EndOfText => (Symbol::new("EOT"), Token::EndOfText),
            t => panic!("I don't know of token {:?}", t),
        }
    }
}