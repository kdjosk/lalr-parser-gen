mod grammar;
use grammar::{Grammar, Symbol, Production};
use std::collections::{HashSet, HashMap};
struct Parser {
    grammar: Grammar,
    first: HashMap<Symbol, HashSet<Symbol>>,
    follow: HashMap<Symbol, HashSet<Symbol>>,
    state_space: Vec<Vec<Production>>,
}

impl Parser {

    fn closure(&self, items: &Vec<Production>) {
        let new_items_added = true;
        while new_items_added {
            for item in items {
                for prod in &self.grammar.productions {

                }
            }
        }
    }



}







