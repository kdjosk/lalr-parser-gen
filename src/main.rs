use lexer::{self, FileSource, Token};
use parser::grammar::{Grammar, Symbol};
use parser::lalr_parsing_tables::{self, LALRParsingTablesGenerator};
use parser::lr_parser::{LRParser, SymbolSource};
use std::env;

struct MockSource {
    symbols: Vec<Symbol>,
}
impl MockSource {
    pub fn new(mut symbols: Vec<Symbol>) -> MockSource {
        symbols.reverse();
        MockSource { symbols }
    }
}
impl SymbolSource for MockSource {
    fn next_symbol(&mut self) -> Symbol {
        self.symbols.pop().unwrap()
    }
}

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
        S' -> S
        S -> L Eq R
        S -> R
        L -> Star R
        L -> id
        R -> L
        "#
        .to_string(),
    );

    let another_grammar = Grammar::new(
        r#"
        S' -> S
        S -> C C
        C -> c C
        C -> d
        "#
        .to_string(),
    );

    let parsing_tables = LALRParsingTablesGenerator::compute(&dflow_grammar);
    let symbol_source = MockSource::new(vec![
        Symbol::new("id"),
        Symbol::new("Eq"),
        Symbol::new("id"),
        Symbol::new("EOT"),
    ]);
    let mut parser = LRParser::new(parsing_tables, symbol_source);
    let output = parser.parse();

    for p in output {
        println!("{}", p);
    }
}
