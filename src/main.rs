use lexer::{self, FileSource, Token, Lexer};
use parser::grammar::{Grammar, Symbol};
use parser::lalr_parsing_tables::{self, LALRParsingTablesGenerator, LALRParsingTables};
use parser::lr_parser::{LRParser, SymbolSource};
use ptree::{TreeBuilder, print_tree};
use std::fs::File;
use std::io::Read;
use std::path::Path;
use parser::lexer_wrapper::LexerWrapper;
use sha2::{Sha256, Digest};
use parser::parse_tree::{Node, Terminal, Nonterminal};
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
    fn next_symbol(&mut self) -> (Symbol, Token) {
        (self.symbols.pop().unwrap(), Token::Empty)
    }
}

fn main() {
    let program_source = FileSource::new("src/program.txt".to_string()).unwrap();
    let lexer = Lexer::new(program_source);

    let mut grammar_input = File::open("src/grammar_input.txt").unwrap();
    let mut buf = String::new();
    grammar_input.read_to_string(&mut buf).unwrap();

    let dflow_grammar = Grammar::new(buf);

    let result = LALRParsingTables::try_load_from_file(
        &Path::new("src/parsing_tables.txt"),
        dflow_grammar.description_hash.clone());

    let parsing_tables;
    if let Ok(Some(t)) = result {
        parsing_tables = t;
        println!("Using cached parsing tables");
    } else {
        println!("Using new parsing tables");
        parsing_tables = LALRParsingTablesGenerator::compute(&dflow_grammar);
        parsing_tables.dump_to_file(&Path::new("src/parsing_tables.txt")).unwrap();
    }

    let symbol_source = LexerWrapper::new(lexer);
    let mut parser = LRParser::new(parsing_tables, symbol_source);
    let (output, root_node) = parser.parse();

    print_ast(&root_node);

}

fn print_ast(root: &Box<dyn Node>) {
    let mut tree = TreeBuilder::new(root.get_label());
    let children = root.get_children();
    match children {
        Some(c) => {
            for child in c.iter().rev() {
                inner_print_ast(child, &mut tree)
            }
            tree.end_child();
        }
        None => ()
    }
    let top_item = tree.build();
    print_tree(&top_item).unwrap();
}

fn inner_print_ast(node: &Box<dyn Node>, tree: &mut TreeBuilder) {
    tree.begin_child(node.get_label());
    let children = node.get_children();
    match children {
        Some(c) => {
            for child in c.iter().rev() {
                inner_print_ast(child, tree)
            }
            tree.end_child();
        }
        None => {
           tree.end_child(); 
        }
    }
}