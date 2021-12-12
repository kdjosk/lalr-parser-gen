use crate::lalr_parsing_tables::{LALRParsingTables, Action};
use crate::grammar::{Symbol, Production};

pub trait SymbolSource {
    fn next_symbol(&mut self) -> Symbol;
}

pub struct LRParser<T: SymbolSource> {
    parsing_tables: LALRParsingTables,
    stack: Vec<usize>,
    output: Vec<Production>,
    symbol_source: T,
}
impl<T: SymbolSource> LRParser<T> {
    pub fn new(parsing_tables: LALRParsingTables, symbol_source: T) -> LRParser<T> {
        LRParser{
            parsing_tables,
            stack: vec![0],
            output: vec![],
            symbol_source,
        }
    }
    
    pub fn parse(&mut self) -> Vec<Production> {
        
        let mut a = self.symbol_source.next_symbol();
        loop {
            let &s = self.stack.last().unwrap();
            let action = self.parsing_tables.get_action(s, &a);
            println!("{:?}, {}, {}", action, s, a);
            match action {
                Action::Shift(t) => {
                    self.stack.push(t);
                    a = self.symbol_source.next_symbol();
                }
                Action::Reduce( p) => {
                    let rhs_len = p.rhs.len();
                    for _ in 0..rhs_len {
                        self.stack.pop().unwrap();
                    }
                    let &t = self.stack.last().unwrap();
                    self.stack.push(self.parsing_tables.get_goto(t, &p.lhs));
                    self.output.push(p.clone());
                },
                Action::Accept => return self.output.clone(),
                Action::Error => {
                    println!("STACK: ");
                    for s in &self.stack {
                        print!("{}, ", s);
                    }
                    panic!("ERROR AT SYMBOL {}", a);
                }
                
            }
        }
    }
}