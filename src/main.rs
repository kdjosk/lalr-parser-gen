use lexer::{self, FileSource, Token};
use parser::Parser;
use parser::grammar::Grammar;
use std::env;

fn main() {
    // let path = env::current_dir().unwrap();
    // let path = path.to_str().unwrap();
    // let source = FileSource::new(format!("{}/{}", path, "snippets_test")).unwrap();
    // let mut lexer = lexer::Lexer::new(source);

    // let mut t = lexer.next_token();
    // while t != Token::EndOfText {
    //     print!("{:?}\n", t);
    //     t = lexer.next_token();
    // }

    let dflow_grammar = Grammar::new(        
        r#"
        S' -> S EOT
        S -> L Eq R
        S -> R
        L -> Star R
        L -> id
        R -> L
        "#.to_string());
    let dflow_parser = Parser::new(dflow_grammar);
    println!("hello");
}
