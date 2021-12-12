mod source;
mod tokens;
use regex::Regex;
pub use source::{FileSource, Source, StringSource};
use std::collections::HashMap;
use std::{mem, panic};
pub use tokens::Token;

pub struct Lexer<T> {
    source: T,
    cache: Option<char>,
    kwords: HashMap<&'static str, Token>,
    ll1_operators: HashMap<&'static str, Token>,
    ll2_operators: HashMap<&'static str, Token>,
}

impl<T: Source> Lexer<T> {
    pub fn new(source: T) -> Lexer<T> {
        Lexer {
            source,
            cache: None,
            kwords: tokens::get_kwords(),
            ll1_operators: tokens::get_ll1_operators(),
            ll2_operators: tokens::get_ll2_operators(),
        }
    }
    // 1 3 2 4
    pub fn next_token(&mut self) -> Token {
        let c = self.peek();

        if self.is_letter_or_underscore(c) {
            return self.build_identifier_or_keyword();
        }
        if self.is_digit(c) {
            return self.build_number_literal();
        }

        Token::Unknown
    }


    fn build_number_literal(&mut self) -> Token {
        match self.peek() {
            '0' => {
                return Token::FloatingLiteral(self.build_floating_literal());
            }
            '1'..='9' => {
                return Token::IntegerLiteral(self.build_integer_literal());
            }
            _ => unreachable!(),
        }
    }

    fn build_floating_literal(&mut self) -> f64 {
        let fractional_part = self.build_fractional_part();
        match self.peek() {
            'e' | 'E' => {
                self.get_char();
                let mut sign = 1.0;
                match self.peek() {
                    '-' => {
                        sign = -1.0;
                        self.get_char();
                    }
                    '+' => {
                        self.get_char();
                    }
                    _ => ()
                }
                let exp = self.build_integer_literal();
                return fractional_part.powf(sign * exp as f64);
            }
            _ => ()
        }
        fractional_part
    }

    fn build_fractional_part(&mut self) -> f64 {
        let mut num: u64 = 0;
        let mut c = self.get_char();
        let mut point_idx = None;
        let mut idx = 0;
        while self.is_digit(c) || c == '.' {
            if c == '.'{
                point_idx = Some(idx);
                continue;
            }
            num += c.to_digit(10).unwrap() as u64;
            num *= 10;
            c = self.get_char();
            idx += 1;
        }
        self.cache(c);

        let mut div: u64 = 1;
        let base: u64 = 10;
        match point_idx {
            Some(i) => div = base.pow(idx - i),
            None => ()
        }

        num as f64 / div as f64
    }

    fn build_integer_literal(&mut self) -> u64 {      
        let mut c = self.get_char();
        let mut num: u64 = c.to_digit(10).unwrap() as u64;
        c = self.get_char();
        while self.is_digit(c) {
            num *= 10;
            num += c.to_digit(10).unwrap() as u64;    
            c = self.get_char();
        }
        self.cache(c);
        num
    }

    fn build_identifier_or_keyword(&mut self) -> Token {
        static MAX_TOKEN_IDENTIFIER_LEN: usize = 255;
        let mut string = String::new();
        string.push(self.get_char());
        let mut c = self.source.get_char();
        while self.is_letter_or_underscore(c) || self.is_digit(c) {
            if string.len() < MAX_TOKEN_IDENTIFIER_LEN {
                panic!("MAX IDENTIFIER LENGTH SURPASSED: {}", string);
            }
            string.push(c);
            c = self.get_char();
        }
        self.cache(c);

        match self.if_is_keyword_get(&string) {
            Some(token) => return token.clone(),
            None => (),
        }

        Token::Identifier(string)
    }

    fn if_is_keyword_get(&self, string: &String) -> Option<&Token> {
        self.kwords.get(string.as_str())
    }

    fn get_char(&mut self) -> char {
        match self.cache {
            Some(c) => {
                self.cache = None;
                c
            }
            None => {
                self.source.get_char()
            }
        }
    }

    fn peek(&mut self) -> char {
        match self.cache {
            Some(c) => {
                c
            }
            None => {
                self.source.peek()
            }
        }
    }


    fn cache(&mut self, c: char) {
        self.cache = Some(c);
    }


    fn is_letter_or_underscore(&self, c: char) -> bool {
        match c {
            'a'..='z' => true,
            'A'..='Z' => true,
            '_' => true,
            _ => false,
        }
    }

    fn is_digit(&self, c: char) -> bool {
        match c {
            '0'..='9' => true,
            _ => false,
        }
    }
}

#[cfg(test)]
mod lexer_tests {
    use super::*;
}
