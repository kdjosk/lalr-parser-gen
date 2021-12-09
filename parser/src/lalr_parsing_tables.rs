use crate::grammar::{Grammar, Production, ProductionWithLookahead, Symbol, DOT, EOT};
use crate::lr0_items::LR0Items;
use lazy_static::lazy_static;
use std::collections::{HashMap, HashSet};

pub struct LALRParsingTables<'a> {
    grammar: &'a Grammar,
}

lazy_static! {
    static ref UNKNOWN: Symbol = Symbol::new("UNKNOWN");
}

struct Lookaheads {
    pub propagated: HashSet<ProductionWithLookahead>,
    pub spontaneous: HashSet<ProductionWithLookahead>,
}
impl Lookaheads {
    pub fn new() -> Lookaheads {
        Lookaheads {
            propagated: HashSet::new(),
            spontaneous: HashSet::new(),
        }
    }
}

struct LookaheadsTable {
    table: Vec<HashMap<Production, Vec<Symbol>>>,
    item_to_set_idx: HashMap<Production, usize>,
}
impl LookaheadsTable {
    pub fn new(kernels: &Vec<HashSet<Production>>) -> LookaheadsTable {
        let mut table = Vec::new();
        let mut item_to_set_idx = HashMap::new();
        for (idx, k) in kernels.iter().enumerate() {
            let mut set = HashSet::new();
            for p in k {
                let el = LookaheadTableElement::new(p.clone());
                set.insert(el);
                item_to_set_idx.insert(p.clone(), idx);
            }
            table.push(set);
        }
        LookaheadsTable {
            table,
            item_to_set_idx
        }     
    }
    
    pub fn add_lookahead(&mut self, item: &Production, lookahead: &Symbol) {
        let &item_idx = self.item_to_set_idx.get(&item).unwrap();
        let current_lookaheads = self.table[item_idx].get(item).unwrap(); //.insert(lookahead.clone());
    }
}

impl<'a> LALRParsingTables<'a> {
    pub fn new(grammar: &'a Grammar) -> LALRParsingTables {
        LALRParsingTables { grammar }
    }

    pub fn compute_tables(&self) {
        let kernels = LR0Items::new(self.grammar).compute_kernels();
        let mut lookaheads_table = self.get_empty_lookaheads_table(&kernels);
        self.make_first_pass_of_propagation(&kernels, &mut lookaheads_table);
        for sym in &self.grammar.symbols {
            let propagated_spontaneous =
                self.discover_propagated_and_spontaneous_lookaheads(&kernels[0], sym);
            println!("Symbol: {}", sym);
            println!("Propagated:");
            for p in propagated_spontaneous.propagated {
                println!("{}", p);
            }
            println!("Spontaneous:");
            for p in propagated_spontaneous.spontaneous {
                println!("{}", p);
            }
        }
    }

    fn make_first_pass_of_propagation(
        &self,
        kernels: &Vec<HashSet<Production>>,
        lookaheads_table: &mut Vec<HashSet<LookaheadTableElement>>,
    ) {
        // contract
        let mut starting_item = self.grammar.get_start_item();
        starting_item.rhs.insert(0, DOT.clone());
        assert_eq!(kernels[0], HashSet::from([starting_item]));
        // --
        for sym in &self.grammar.symbols {
            let lookaheads =
                self.discover_propagated_and_spontaneous_lookaheads(&kernels[0], sym);
            for p in lookaheads.spontaneous {
                
            }
        }
    }

    // page 272 in the Dragon Book
    fn discover_propagated_and_spontaneous_lookaheads(
        &self,
        lr0_kernel: &HashSet<Production>,
        sym: &Symbol,
    ) -> Lookaheads {
        let mut lookaheads = Lookaheads::new();
        for item in lr0_kernel {
            let closure_set =
                self.closure_with_lookahead(&HashSet::from([ProductionWithLookahead::new(
                    item.clone(),
                    UNKNOWN.clone(),
                )]));
            for item in &closure_set {
                let dot_idx = item.production.find_indexes_of_symbol_on_rhs(&DOT);
                assert_eq!(dot_idx.len(), 1);
                let dot_idx = dot_idx[0];

                if dot_idx < item.production.rhs.len() - 1
                    && &item.production.rhs[dot_idx + 1] == sym
                {
                    let mut goto_item = item.clone();
                    goto_item.production.rhs.swap(dot_idx, dot_idx + 1);

                    if item.lookahead != UNKNOWN.clone() {
                        lookaheads.spontaneous.insert(goto_item);
                    } else {
                        lookaheads.propagated.insert(goto_item);
                    }
                }
            }
        }
        lookaheads
    }

    // page 261
    fn goto_with_lookahead(
        &self,
        set_of_items: &HashSet<ProductionWithLookahead>,
        symbol: &Symbol,
    ) -> HashSet<ProductionWithLookahead> {
        let mut new_set = HashSet::new();
        for item in set_of_items {
            match self.move_dot_through(&item.production, symbol) {
                Some(i) => {
                    new_set.insert(ProductionWithLookahead {
                        production: i,
                        lookahead: item.lookahead.clone(),
                    });
                }
                None => (),
            };
        }
        self.closure_with_lookahead(&new_set)
    }

    fn move_dot_through(&self, item: &Production, symbol: &Symbol) -> Option<Production> {
        let dot_idx = item.find_indexes_of_symbol_on_rhs(&*DOT);
        assert_eq!(dot_idx.len(), 1);
        let dot_idx = dot_idx[0];
        let sym_idxs = item.find_indexes_of_symbol_on_rhs(symbol);
        let mut new_prod = item.clone();

        for idx in sym_idxs {
            if idx >= 1 && idx - 1 == dot_idx {
                new_prod.rhs.insert(idx + 1, DOT.clone());
                new_prod.rhs.remove(dot_idx);
                return Some(new_prod);
            }
        }
        None
    }

    // page 261
    fn closure_with_lookahead(
        &self,
        items: &HashSet<ProductionWithLookahead>,
    ) -> HashSet<ProductionWithLookahead> {
        let mut set_of_items: HashSet<ProductionWithLookahead> =
            items.clone().into_iter().collect();
        let mut new_set_of_items = set_of_items.clone();
        let mut new_items_added;

        loop {
            new_items_added = false;
            for item in &set_of_items {
                let dot_idx = item.production.find_indexes_of_symbol_on_rhs(&*DOT);
                assert_eq!(dot_idx.len(), 1);
                let dot_idx = dot_idx[0];
                assert!(dot_idx < item.production.rhs.len());
                if dot_idx == item.production.rhs.len() - 1 {
                    continue;
                }
                let sym_after_dot = &item.production.rhs[dot_idx + 1];
                if self.grammar.is_terminal(sym_after_dot) {
                    continue;
                }

                // beta in book
                let mut postfix;
                if dot_idx + 2 >= item.production.rhs.len() {
                    postfix = vec![];
                } else {
                    let rhs = item.production.rhs.clone();
                    postfix = rhs[dot_idx + 2..].to_vec();
                }
                postfix.push(item.lookahead.clone());

                for prod in self.grammar.get_productions_of(sym_after_dot) {
                    for terminal in self.grammar.first(&postfix) {
                        let mut p = prod.clone();
                        p.rhs.insert(0, DOT.clone());
                        let pa = ProductionWithLookahead {
                            production: p,
                            lookahead: terminal,
                        };
                        if !set_of_items.contains(&pa) && new_set_of_items.insert(pa) {
                            new_items_added = true;
                        }
                    }
                }
            }
            if !new_items_added {
                break;
            }

            set_of_items.extend(new_set_of_items);
            new_set_of_items = HashSet::new();
        }

        set_of_items
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils;
    use std::hash::Hash;

    #[test]
    fn lookaheads_test() {
        let descript = test_utils::get_test_grammar_2_description();
        let grammar = Grammar::new(descript);
        let lalr = LALRParsingTables::new(&grammar);
    }

    #[test]
    fn closure_with_lookahead_test() {
        let descript = test_utils::get_test_grammar_3_description();

        let grammar = Grammar::new(descript);
        let mut starting_item = grammar.productions[0].clone();
        starting_item.rhs.insert(0, DOT.clone());
        let starting_item = ProductionWithLookahead::new(starting_item, EOT.clone());
        let item_set = HashSet::from([starting_item]);
        let lalr = LALRParsingTables::new(&grammar);
        let closure = lalr.closure_with_lookahead(&item_set);

        for p in &closure {
            println!("{}", p);
        }

        let expected = HashSet::from([
            ProductionWithLookahead::from("S' -> . S EOT", "EOT"),
            ProductionWithLookahead::from("S -> . C C", "EOT"),
            ProductionWithLookahead::from("C -> . c C", "c"),
            ProductionWithLookahead::from("C -> . c C", "d"),
            ProductionWithLookahead::from("C -> . d", "c"),
            ProductionWithLookahead::from("C -> . d", "d"),
        ]);

        assert_eq!(closure, expected);
    }

    #[test]
    fn goto_with_lookahead_test() {
        let descript = test_utils::get_test_grammar_3_description();
        let grammar = Grammar::new(descript);
        let mut starting_item = grammar.productions[0].clone();
        starting_item.rhs.insert(0, DOT.clone());

        let mut starting_item = ProductionWithLookahead::new(starting_item, EOT.clone());
        let lalr = LALRParsingTables::new(&grammar);
        let starting_item_closure = lalr.closure_with_lookahead(&HashSet::from([starting_item]));
        let value = lalr.goto_with_lookahead(&starting_item_closure, &Symbol::new("C"));
        for p in &value {
            println!("{}", p);
        }

        // page 262
        let expected = HashSet::from([
            ProductionWithLookahead::from("S -> C . C", "EOT"),
            ProductionWithLookahead::from("C -> . c C", "EOT"),
            ProductionWithLookahead::from("C -> . d", "EOT"),
        ]);
        assert_eq!(value, expected)
    }
}
