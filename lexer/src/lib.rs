mod source;
mod tokens;
use regex::Regex;
pub use source::{FileSource, Source, StringSource};
use std::collections::HashMap;
use std::{mem, panic};
pub use tokens::{Token, KEYWORDS, ONE_CHAR_OPERATORS, TWO_CHAR_OPERATORS};

pub struct Lexer<T> {
    source: T,
    cache: Option<char>,
}

impl<T: Source> Lexer<T> {
    pub fn new(source: T) -> Lexer<T> {
        Lexer {
            source,
            cache: None,
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
        if let Some(t) = self.try_build_string_literal() {
            return t;
        }
        if let Some(t) = self.try_build_operator() {
            return t;
        }
        if let Some(t) = self.try_build_whitespace() {
            return t;
        }
        unreachable!("Could not recognize token!")
    }

    fn try_build_string_literal(&mut self) -> Option<Token> {
        static MAX_STRING_LITERAL_LEN: usize = 8192;
        let opening = self.peek();
        match opening {
            '\'' | '"' => {
                self.get_char();
                let mut literal = String::new();
                while literal.len() < MAX_STRING_LITERAL_LEN {
                    let c = self.get_char();
                    match c {
                        '\'' | '"' => {
                            if c == opening {
                                return Some(Token::StringLiteral(literal));
                            }
                            panic!("Opening is `{}`, but ending is `{}`", opening, c);
                        }
                        '\\' => {
                            let c = self.get_char();
                            match c {
                                '\'' | '"' | '\\' => literal.push(c),
                                't' => literal.push('\t'),
                                'n' => literal.push('\n'),
                                _ => panic!("Unknown escape sequence \\{}", c),
                            }
                        }
                        '$' => {
                            panic!("No matching ending for opening `{}`", opening);
                        }
                        c => {
                            literal.push(c);
                        }
                    }
                }
                panic!("Max string literal len exceeded");
            }
            _ => None,
        }
    }

    fn try_build_whitespace(&mut self) -> Option<Token> {
        let c = self.get_char();
        match c {
            '\n' => Some(Token::Newline),
            '\t' => Some(Token::Tab),
            ' ' => Some(Token::Space),
            _ => {
                self.cache(c);
                None
            } 
        }
        
    }

    fn try_build_operator(&mut self) -> Option<Token> {
        let c = self.get_char();
        let mut op = String::new();
        op.push(c);
        op.push(self.peek());
        if TWO_CHAR_OPERATORS.contains_key(op.as_str()) {
            return Some(TWO_CHAR_OPERATORS.get(op.as_str()).unwrap().clone());
        }
        op.pop();

        if ONE_CHAR_OPERATORS.contains_key(op.as_str()) {
            Some(ONE_CHAR_OPERATORS.get(op.as_str()).unwrap().clone())
        } else {
            self.cache(c);
            None
        }
    }

    fn build_number_literal(&mut self) -> Token {
        match self.peek() {
            '0' => {
                self.get_char();
                match self.peek() {
                    '.' => Token::FloatingLiteral(self.build_floating_literal(0)),
                    '0'..='9' => panic!("Invalid number prefix 0[0-9]"),
                    _ => Token::IntegerLiteral(0),
                }
            }
            '1'..='9' => {
                let integer = self.build_integer_literal();
                match self.peek() {
                    '.' => {
                        let floating = self.build_floating_literal(integer);
                        Token::FloatingLiteral(floating)
                    }
                    'e' | 'E' => {
                        let exp = self.build_exponent_part();
                        let res = integer as f64 * exp;
                        Token::FloatingLiteral(res)
                    }
                    _ => Token::IntegerLiteral(integer),
                }
            }
            _ => unreachable!(),
        }
    }

    fn build_floating_literal(&mut self, integer_prefix: u64) -> f64 {
        let fractional_part = self.build_fractional_part();
        match self.peek() {
            'e' | 'E' => {
                let exponent = self.build_exponent_part();
                return (integer_prefix as f64 + fractional_part) * exponent;
            }
            _ => (),
        }
        integer_prefix as f64 + fractional_part
    }

    fn build_exponent_part(&mut self) -> f64 {
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
            _ => (),
        }
        let exp = self.build_integer_literal();
        let base: f64 = 10.0;
        return base.powf(sign * exp as f64);
    }

    fn build_fractional_part(&mut self) -> f64 {
        let mut num: u64 = 0;
        let mut c = self.get_char();
        let mut point_idx = None;
        let mut idx = 0;
        while self.is_digit(c) || c == '.' {
            if c == '.' {
                point_idx = Some(idx);
                idx += 1;
                c = self.get_char();
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
            None => (),
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
            if string.len() >= MAX_TOKEN_IDENTIFIER_LEN {
                panic!("MAX IDENTIFIER LENGTH SURPASSED: {}{}", string, c);
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
        KEYWORDS.get(string.as_str())
    }

    fn get_char(&mut self) -> char {
        match self.cache {
            Some(c) => {
                self.cache = None;
                c
            }
            None => self.source.get_char(),
        }
    }

    fn peek(&mut self) -> char {
        match self.cache {
            Some(c) => c,
            None => self.source.peek(),
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
