use lexer::{self, FileSource, Token};
use std::env;

fn main() {
    let path = env::current_dir().unwrap();
    let path = path.to_str().unwrap();
    let source = FileSource::new(format!("{}/{}", path, "snippets_test")).unwrap();
    let mut lexer = lexer::Lexer::new(source);

    let mut t = lexer.next_token();
    while t != Token::EndOfText {
        print!("{:?}\n", t);
        t = lexer.next_token();
    }
}

