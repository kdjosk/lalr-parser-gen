mod source;
mod tokens;
use regex::Regex;
pub use source::{FileSource, Source, StringSource};
use std::collections::HashMap;
use std::mem;
pub use tokens::Token;

pub struct Lexer<T> {
    source: T,
    kwords: HashMap<&'static str, Token>,
    ll1_operators: HashMap<&'static str, Token>,
    ll2_operators: HashMap<&'static str, Token>,
    rules: LexerRules,
    token_string: String,
    old_token_string: String,
}

struct LexerRules {
    whitespace: Regex,
    comment: Regex,
    identifier: Regex,
    string_literal: Regex,
    integer_literal: Regex,
    floating_literal: Regex,
    end_of_text: Regex,
}

impl LexerRules {
    pub fn new() -> LexerRules {
        LexerRules {
            whitespace: Regex::new(r"^[[:space:]]+$").unwrap(),
            comment: Regex::new(r"^#.*(\n)?$").unwrap(),
            identifier: Regex::new(r"^[a-zA-z_]([a-zA-z_0-9])*$").unwrap(),
            string_literal: Regex::new(r#"^"(\\.|[^"\\])*"$"#).unwrap(),
            integer_literal: Regex::new(r"^0$|^[1-9]('?[0-9])*$").unwrap(),
            floating_literal: Regex::new(
                "\
                ^[1-9]('?[0-9])*e(\\+|-)?(0|[1-9]('?[0-9])*)$|\
                ^(0|[1-9]('?[0-9])*)\\.('?[0-9])*$|\
                ^(0|[1-9]('?[0-9])*)\\.('?[0-9])*e(\\+|-)?(0|[1-9]('?[0-9])*)$",
            )
            .unwrap(),
            end_of_text: Regex::new(r"^\$$").unwrap(),
        }
    }
}

impl<T: Source> Lexer<T> {
    pub fn new(source: T) -> Lexer<T> {
        Lexer {
            source,
            kwords: tokens::get_kwords(),
            ll1_operators: tokens::get_ll1_operators(),
            ll2_operators: tokens::get_ll2_operators(),
            rules: LexerRules::new(),
            token_string: String::new(),
            old_token_string: String::new(),
        }
    }

    pub fn next_token(&mut self) -> Token {
        static MAX_TOKEN_STRING_LEN: usize = 255;

        while self.token_string.len() <= MAX_TOKEN_STRING_LEN {
            self.old_token_string = self.token_string.clone();
            let c = self.source.get_char();
            self.token_string.push(c);

            match self.match_keyword() {
                Some(t) => return t,
                None => (),
            }

            match self.match_ll1_operator() {
                Some(t) => return t,
                None => (),
            }

            match self.match_ll2_operator() {
                Some(t) => return t,
                None => (),
            }

            if self.match_whitespace() {
                continue;
            }

            if self.match_comment() {
                continue;
            }

            match self.match_identifier() {
                Some(t) => return t,
                None => (),
            }

            match self.match_string_literal() {
                Some(t) => return t,
                None => (),
            }

            match self.match_integer_literal(c) {
                Some(t) => return t,
                None => (),
            }

            match self.match_floating_literal() {
                Some(t) => return t,
                None => (),
            }

            match self.match_end_of_text(c) {
                Some(t) => return t,
                None => (),
            }
        }
        unreachable!("Can't recognize token {}", self.token_string);
    }

    fn is_matching_value(&self, rule: &Regex) -> bool {
        !rule.is_match(self.token_string.as_str()) && rule.is_match(self.old_token_string.as_str())
    }

    fn reset_token_strings_but_leave_last_char_of_token_string(&mut self) {
        self.token_string = self.token_string.pop().unwrap().to_string();
        self.old_token_string = String::new();
    }

    fn match_keyword(&mut self) -> Option<Token> {
        match self.kwords.get(self.old_token_string.as_str()) {
            Some(t) => {
                println!("Matched keyword {}", self.old_token_string);
                self.token_string = self.token_string.pop().unwrap().to_string();
                self.old_token_string = String::new();
                Some(t.clone())
            }
            None => None,
        }
    }

    fn match_ll1_operator(&mut self) -> Option<Token> {
        match self.ll1_operators.get(self.old_token_string.as_str()) {
            Some(t) => {
                println!("Matched ll1 operator {}", self.old_token_string);
                self.token_string = self.token_string.pop().unwrap().to_string();
                self.old_token_string = String::new();
                Some(t.clone())
            }
            None => None,
        }
    }

    fn match_whitespace(&mut self) -> bool {
        if self.is_matching_value(&self.rules.whitespace) {
            println!("Matched whitespace `{}`", self.old_token_string);
            self.reset_token_strings_but_leave_last_char_of_token_string();
            return true;
        }
        false
    }

    fn match_comment(&mut self) -> bool {
        if self.is_matching_value(&self.rules.comment) {
            println!("Matched comment {}", self.old_token_string);
            self.reset_token_strings_but_leave_last_char_of_token_string();
            return true;
        }
        false
    }

    fn match_identifier(&mut self) -> Option<Token> {
        if self.is_matching_value(&self.rules.identifier) {
            println!("Matched identifier {}", self.old_token_string);
            let token = Token::Identifier(mem::replace(&mut self.old_token_string, String::new()));
            self.reset_token_strings_but_leave_last_char_of_token_string();
            return Some(token);
        }
        None
    }

    fn match_string_literal(&mut self) -> Option<Token> {
        if self.is_matching_value(&self.rules.string_literal) {
            println!("Matched string literal {}", self.old_token_string);
            let token =
                Token::StringLiteral(mem::replace(&mut self.old_token_string, String::new()));
            self.reset_token_strings_but_leave_last_char_of_token_string();
            return Some(token);
        }
        None
    }

    fn match_integer_literal(&mut self, new: char) -> Option<Token> {
        if self.is_matching_value(&self.rules.integer_literal)
            && new != 'e'
            && new != '.'
            && new != '\''
        {
            println!("Matched integer literal {}", self.old_token_string);
            let token =
                Token::IntegerLiteral(mem::replace(&mut self.old_token_string, String::new()));
            self.reset_token_strings_but_leave_last_char_of_token_string();
            return Some(token);
        }
        None
    }

    fn match_floating_literal(&mut self) -> Option<Token> {
        if self.is_matching_value(&self.rules.floating_literal) {
            println!("Matched floating_literal {}", self.old_token_string);
            let token =
                Token::FloatingLiteral(mem::replace(&mut self.old_token_string, String::new()));
            self.reset_token_strings_but_leave_last_char_of_token_string();
            return Some(token);
        }
        None
    }

    fn match_ll2_operator(&mut self) -> Option<Token> {
        if self.token_string.len() == 2 && self.old_token_string.len() == 1 {
            match self.ll2_operators.get(self.token_string.as_str()) {
                Some(t) => {
                    println!("Matched ll2 operator {}", self.token_string);
                    self.token_string = String::new();
                    self.old_token_string = String::new();
                    return Some(t.clone());
                }
                None => (),
            }
            match self.ll2_operators.get(self.old_token_string.as_str()) {
                Some(t) => {
                    println!("Matched ll2 operator {}", self.old_token_string);
                    self.token_string = self.token_string.pop().unwrap().to_string();
                    self.old_token_string = String::new();
                    return Some(t.clone());
                }
                None => return None,
            }
        }
        None
    }

    fn match_end_of_text(&self, new: char) -> Option<Token> {
        if self.rules.end_of_text.is_match(new.to_string().as_str()) {
            println!("Matched end of text");
            return Some(Token::EndOfText);
        }
        None
    }
}

#[cfg(test)]
mod lexer_tests {
    use super::*;

    #[test]
    fn integer_literal() {
        LexerTest {
            program: "let x = 5;".to_string(),
            tokens: vec![
                Token::Let,
                Token::Identifier("x".to_string()),
                Token::Assign,
                Token::IntegerLiteral("5".to_string()),
                Token::Semi,
                Token::EndOfText,
            ],
        }
        .run();
    }

    #[test]
    fn arithmetic() {
        LexerTest {
            program: "5 + 10e2 * 30 / 1.2".to_string(),
            tokens: vec![
                Token::IntegerLiteral("5".to_string()),
                Token::Plus,
                Token::FloatingLiteral("10e2".to_string()),
                Token::Star,
                Token::IntegerLiteral("30".to_string()),
                Token::Div,
                Token::FloatingLiteral("1.2".to_string()),
                Token::EndOfText,
            ],
        }
        .run();
    }

    #[test]
    fn boolean_expr() {
        LexerTest {
            program: "not is_ok and true or false".to_string(),
            tokens: vec![
                Token::Not,
                Token::Identifier("is_ok".to_string()),
                Token::And,
                Token::True,
                Token::Or,
                Token::False,
                Token::EndOfText,
            ],
        }
        .run();
    }

    #[test]
    fn string_literal() {
        LexerTest {
            program: "let s = \"Hello world!\n\";".to_string(),
            tokens: vec![
                Token::Let,
                Token::Identifier("s".to_string()),
                Token::Assign,
                Token::StringLiteral("\"Hello world!\n\"".to_string()),
                Token::Semi,
                Token::EndOfText,
            ],
        }
        .run();
    }

    #[test]
    fn dflow() {
        LexerTest {
            program: "dflow Hello {
                    name_collector() -> Name;
                    greeting_printer(Name);
                }"
            .to_string(),
            tokens: vec![
                Token::Dflow,
                Token::Identifier("Hello".to_string()),
                Token::LBrace,
                Token::Identifier("name_collector".to_string()),
                Token::LParen,
                Token::RParen,
                Token::RArrow,
                Token::Identifier("Name".to_string()),
                Token::Semi,
                Token::Identifier("greeting_printer".to_string()),
                Token::LParen,
                Token::Identifier("Name".to_string()),
                Token::RParen,
                Token::Semi,
                Token::RBrace,
                Token::EndOfText,
            ],
        }
        .run();
    }

    #[test]
    fn component() {
        LexerTest {
            program: "component Gps {
                    _init(ip: str)
                    : pre (
                      IsValidIp(ip))
                    : post (
                      m.IsConnected()) {
                    }
                  
                    _call() -> GpsOutput {
                      return GpsOutput(
                        position = vec<f32>(0, 0, 0)
                    }
                  }"
            .to_string(),
            tokens: vec![
                Token::Component,
                Token::Identifier("Gps".to_string()),
                Token::LBrace,
                Token::Init,
                Token::LParen,
                Token::Identifier("ip".to_string()),
                Token::Colon,
                Token::String,
                Token::RParen,
                Token::Colon,
                Token::Pre,
                Token::LParen,
                Token::Identifier("IsValidIp".to_string()),
                Token::LParen,
                Token::Identifier("ip".to_string()),
                Token::RParen,
                Token::RParen,
                Token::Colon,
                Token::Post,
                Token::LParen,
                Token::M,
                Token::Dot,
                Token::Identifier("IsConnected".to_string()),
                Token::LParen,
                Token::RParen,
                Token::RParen,
                Token::LBrace,
                Token::RBrace,
                Token::Call,
                Token::LParen,
                Token::RParen,
                Token::RArrow,
                Token::Identifier("GpsOutput".to_string()),
                Token::LBrace,
                Token::Return,
                Token::Identifier("GpsOutput".to_string()),
                Token::LParen,
                Token::Identifier("position".to_string()),
                Token::Assign,
                Token::Vec,
                Token::Less,
                Token::F32,
                Token::Greater,
                Token::LParen,
                Token::IntegerLiteral("0".to_string()),
                Token::Coma,
                Token::IntegerLiteral("0".to_string()),
                Token::Coma,
                Token::IntegerLiteral("0".to_string()),
                Token::RParen,
                Token::RBrace,
                Token::RBrace,
                Token::EndOfText,
            ],
        }
        .run();
    }

    struct LexerTest {
        program: String,
        tokens: Vec<Token>,
    }

    impl LexerTest {
        pub fn run(&mut self) {
            use std::mem;
            let source = StringSource::new(mem::replace(&mut self.program, String::new()));

            let mut lex = Lexer::new(source);

            for t in &self.tokens {
                assert_eq!(lex.next_token(), *t);
            }
        }
    }
}

#[cfg(test)]
mod lexer_rule_tests {

    use super::*;

    #[test]
    fn comment_rule() {
        let rules = LexerRules::new();
        RegexTest {
            regex: &rules.comment,
            should_match: vec!["# comment\n", "# comment # comment", "# comment"],
            no_match: vec!["# comment\n non comment", "non comment # comment"],
        }
        .run();
    }

    #[test]
    fn whitespace_rule() {
        let rules = LexerRules::new();
        RegexTest {
            regex: &rules.whitespace,
            should_match: vec![" ", "\n", "\t", "\r"],
            no_match: vec![],
        }
        .run();
    }

    #[test]
    fn identifier_rule() {
        let rules = LexerRules::new();
        RegexTest {
            regex: &rules.identifier,
            should_match: vec!["x", "a", "agfjdks3242", "fjkdsl33jkfslj3kljr32klf3", "XYZ"],
            no_match: vec!["93fds"],
        }
        .run();
    }

    #[test]
    fn string_literal_rule() {
        let rules = LexerRules::new();
        RegexTest {
            regex: &rules.string_literal,
            should_match: vec!["\"I'm a \nstring literal\n\"", "\"Hello world!\""],
            no_match: vec!["I'm not a \nstring literal\n", "\"Hello world!\";"],
        }
        .run();
    }

    #[test]
    fn integer_literal_rule() {
        let rules = LexerRules::new();
        RegexTest {
            regex: &rules.integer_literal,
            should_match: vec!["139281038", "1", "3'232'232'32", "232", "0"],
            no_match: vec!["0132", "0030213", "'232544", "3232'", "-23", "-232"],
        }
        .run();
    }

    #[test]
    fn floating_literal_rule() {
        let rules = LexerRules::new();
        RegexTest {
            regex: &rules.floating_literal,
            should_match: vec![
                "139281038e9",
                "1e9",
                "3'232'232'32e-9",
                "2322123e+8",
                "2132.23",
                "0.2323",
                "0.0",
                "32.00001",
                "2132.23e13",
                "0.2323e0",
                "0.0e10",
                "32.00001e-43",
            ],
            no_match: vec![
                "0132e 9", "0030213.", ".232544", "32,32", "23e+-9", "2320e01", "14",
            ],
        }
        .run();
    }

    struct RegexTest<'a> {
        regex: &'a Regex,
        should_match: Vec<&'a str>,
        no_match: Vec<&'a str>,
    }

    impl<'a> RegexTest<'a> {
        pub fn run(&self) {
            for &t in &self.should_match {
                assert!(
                    self.regex.is_match(t),
                    "Failed to match `{}`",
                    t.escape_default()
                );
            }
            for &t in &self.no_match {
                assert!(
                    !self.regex.is_match(t),
                    "Matched an incorrect sequence `{}`",
                    t.escape_default()
                );
            }
        }
    }
}
