use lexer::{self, FileSource, Token, Lexer};
use parser::grammar::{Grammar, Symbol};
use parser::lalr_parsing_tables::{self, LALRParsingTablesGenerator};
use parser::lr_parser::{LRParser, SymbolSource};
use std::env;
use std::fs::File;
use std::io::Read;
use dflow::LexerWrapper;

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
    let program_source = FileSource::new("src/program.txt".to_string()).unwrap();
    let lexer = Lexer::new(program_source);

    let mut grammar_input = File::open("src/simple_grammar_input.txt").unwrap();
    let mut buf = String::new();
    grammar_input.read_to_string(&mut buf).unwrap();

    let dflow_grammar = Grammar::new(buf);

    let parsing_tables = LALRParsingTablesGenerator::compute(&dflow_grammar);

    let symbol_source = LexerWrapper::new(lexer);
    let mut parser = LRParser::new(parsing_tables, symbol_source);
    let output = parser.parse();

    for p in output {
        println!("{}", p);
    }
}
