use generic_array::typenum::Integer;
use lexer::Token;
use crate::grammar::{Production, Symbol};
use crate::lalr_parsing_tables::{Action, LALRParsingTables};
use crate::abstract_syntax_tree::{Leaf, Interior, Node};


pub trait SymbolSource {
    fn next_symbol(&mut self) -> (Symbol, Token);
}

struct StackElement {
    state: usize,
    parse_tree_node: Box<dyn Node>,
}
impl StackElement {
    pub fn new(state: usize, parse_tree_node: Box<dyn Node>) -> StackElement {
        StackElement{
            state,
            parse_tree_node,
        }
    }

    pub fn starting() -> StackElement {
        StackElement {
            state: 0,
            parse_tree_node: Box::new(Interior::new("program".to_string(), vec![])),
        }
    }
}

pub struct LRParser<T: SymbolSource> {
    parsing_tables: LALRParsingTables,
    stack: Vec<StackElement>,
    output: Vec<Production>,
    symbol_source: T,
}
impl<T: SymbolSource> LRParser<T> {
    pub fn new(parsing_tables: LALRParsingTables, symbol_source: T) -> LRParser<T> {
        LRParser {
            parsing_tables,
            stack: vec![StackElement::starting()],
            output: vec![],
            symbol_source,
        }
    }

    pub fn parse(&mut self) -> (Vec<Production>, Box<dyn Node>) {
        let (mut terminal, mut token) = self.symbol_source.next_symbol();
        loop {
            let action = self.parsing_tables.get_action(
                self.stack.last().unwrap().state,
                &terminal);
            match action {
                Action::Shift(state) => {
                    self.stack.push(
                        StackElement::new(
                            state, 
                            match token {
                                Token::IntegerLiteral(i) => Box::new(Leaf::new(terminal.id, i)),
                                Token::StringLiteral(s) => Box::new(Leaf::new(terminal.id, s)),
                                Token::FloatingLiteral(f) => Box::new(Leaf::new(terminal.id, f)),
                                Token::Identifier(id) => Box::new(Leaf::new(terminal.id, id)),
                                _ => Box::new(Leaf::new(terminal.id, ())),
                            }
                        )
                    );
                    let temp = self.symbol_source.next_symbol();
                    terminal = temp.0;
                    token = temp.1;
                }
                Action::Reduce(p) => {
                    let rhs_len = p.rhs.len();
                    let mut parse_tree_node = Interior::new(p.lhs.id.clone(), vec![]);
                    for _ in 0..rhs_len {
                        let stack_el = self.stack.pop().unwrap();
                        parse_tree_node.add_child(stack_el.parse_tree_node);
                    }

                    let goto_state = self.parsing_tables.get_goto(
                        self.stack.last().unwrap().state, 
                        &p.lhs
                    );
                    self.stack.push(
                        StackElement::new(
                            goto_state,
                            Box::new(parse_tree_node),
                        )
                    );
                    self.output.push(p.clone());
                }
                Action::Accept => {
                    return (self.output.clone(), self.stack.pop().unwrap().parse_tree_node);
                } 
                Action::Error => {
                    println!("STACK: ");
                    for s in &self.stack {
                        print!("{}, ", s.state);
                    }
                    panic!("ERROR AT SYMBOL {}", terminal);
                }
            }
        }
    }
}

