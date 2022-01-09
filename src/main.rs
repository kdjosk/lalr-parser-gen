use lexer::{self, FileSource, Lexer, Token};
use parser::grammar::{Grammar, Symbol};
use parser::lalr_parsing_tables::{self, LALRParsingTables, LALRParsingTablesGenerator};
use parser::lexer_wrapper::LexerWrapper;
use parser::lr_parser::{LRParser, SymbolSource};
use parser::parse_tree::{Labeled, Nonterminal, Terminal, ParseTreeNode};
use ptree::{print_tree, TreeBuilder};
use sha2::{Digest, Sha256};
use std::fs::File;
use std::io::Read;
use std::path::Path;
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
        dflow_grammar.description_hash.clone(),
    );

    let parsing_tables;
    if let Ok(Some(t)) = result {
        parsing_tables = t;
        println!("Using cached parsing tables");
    } else {
        println!("Using new parsing tables");
        parsing_tables = LALRParsingTablesGenerator::compute(&dflow_grammar);
        parsing_tables
            .dump_to_file(&Path::new("src/parsing_tables.txt"))
            .unwrap();
    }

    let symbol_source = LexerWrapper::new(lexer);
    let mut parser = LRParser::new(parsing_tables, symbol_source);
    let (output, root_node) = parser.parse();

    print_ast(&root_node);
}

fn print_ast(root: &ParseTreeNode) {
    match root {
        ParseTreeNode::Internal(node) => {
            let mut tree = TreeBuilder::new(node.get_label());
            let children = node.children_ref();
            for child in children {
                inner_print_ast(child, &mut tree)
            }
            tree.end_child();
            let top_item = tree.build();
            print_tree(&top_item).unwrap();
        }
        ParseTreeNode::Leaf(_) => panic!("Leaf node can't be a root"),
    }
   
}

fn inner_print_ast(node: &ParseTreeNode, tree: &mut TreeBuilder) {
    match node {
        ParseTreeNode::Internal(n) => {
            tree.begin_child(n.get_label());
            let children = n.children_ref();
            for child in children {
                inner_print_ast(child, tree)
            }
            tree.end_child();
        }
        ParseTreeNode::Leaf(n) => {
            tree.begin_child(n.get_label());
            tree.end_child();
        }
    }
}
