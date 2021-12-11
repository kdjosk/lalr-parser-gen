use crate::grammar::{Grammar, Production, ProductionWithLookahead, Symbol, DOT, EOT};
use crate::lr0_items::LR0Items;
use lazy_static::lazy_static;
use std::collections::btree_map::OccupiedEntry;
use std::collections::{HashMap, HashSet};
use std::mem;

pub struct LALRParsingTables<'a> {
    grammar: &'a Grammar,
}

lazy_static! {
    static ref UNKNOWN: Symbol = Symbol::new("UNKNOWN");
}

#[derive(PartialEq, Eq, Hash, Clone)]
struct SetItem {
    production: Production,
    set_idx: usize,
}
impl SetItem {
    pub fn new(production: Production, set_idx: usize) -> SetItem {
        SetItem { production, set_idx }
    }
}

struct LookaheadsTable {
    table: Vec<HashMap<Production, HashSet<Symbol>>>,
    table_swap_copy: Vec<HashMap<Production, HashSet<Symbol>>>,
    propagation: HashMap<SetItem, HashSet<SetItem>>,
}
impl LookaheadsTable {
    pub fn new(kernels: &Vec<HashSet<Production>>) -> LookaheadsTable {
        let mut table = Vec::new();
        let mut item_to_set_idx = HashMap::new();
        let mut propagation = HashMap::new();
        for (idx, k) in kernels.iter().enumerate() {
            let mut table_el = HashMap::new();
            for p in k {
                table_el.insert(p.clone(), HashSet::new());
                item_to_set_idx.insert(p.clone(), idx);
                propagation.insert(SetItem::new(p.clone(), idx), HashSet::new());
            }
            table.push(table_el);
        }
        LookaheadsTable {
            table_swap_copy: table.clone(),
            table,
            propagation,
        }     
    }
    
    pub fn add_lookahead(&mut self, item: &SetItem, lookahead: &Symbol) {
        let current_lookaheads = self.table[item.set_idx].get_mut(&item.production).unwrap();
        current_lookaheads.insert(lookahead.clone());
    }

    pub fn make_repeated_passes_of_propagation(
        &mut self,
        kernels: &Vec<HashSet<Production>>,
    ) {
        let mut should_continue = true;
        for i in [..10] {
            should_continue = false;
            for (set_idx, kernel) in kernels.iter().enumerate() {
                for kernel_item in kernel {
                    if self.propagate_lookaheads(&SetItem::new(kernel_item.clone(), set_idx)) {
                        should_continue = true;
                    }
                }
            }
            self.swap_table_after_propagation_pass();
        }
        
    }

    pub fn swap_table_after_propagation_pass(&mut self) {
        mem::swap(&mut self.table, &mut self.table_swap_copy)
    }
    
    // TODO - Gets stuck on pass 1 state
    pub fn propagate_lookaheads(&mut self, from: &SetItem) -> bool {
        let from_lookaheads = self.table[from.set_idx].get(&from.production).unwrap().clone();
        self.table_swap_copy[from.set_idx].get_mut(&from.production).unwrap().extend(from_lookaheads.clone());
        let mut new_propagations = false;
        let propagations = self.propagation.get(from).unwrap();

        for item in propagations {     
            let item_lookaheads = self.table_swap_copy[item.set_idx].get_mut(&item.production).unwrap();
            if *item_lookaheads != from_lookaheads {
                new_propagations = true;
                item_lookaheads.extend(from_lookaheads.clone())
            }     
        }
        new_propagations
    }

    pub fn add_propagation(&mut self, from: &SetItem, to: &SetItem) {
        let targets = self.propagation.get_mut(from).unwrap();
        targets.insert(to.clone());
    }

    pub fn print(&self) {
        for (idx, map) in self.table.iter().enumerate() {
            println!("Set: {}", idx);
            for (prod, lookaheads) in map.iter() {
                print!("{} | ", prod);
                for l in lookaheads {
                    print!("{}, ", l);
                }
                print!("\n");
            }
        }
        println!("propagations:");
        for (from, to) in self.propagation.iter() {
            println!("FROM: {} IN {}", from.production, from.set_idx);
            println!("TO: ");
            for p in to {
                println!("{} in {}", p.production, p.set_idx);
            }
        }

    }
}

impl<'a> LALRParsingTables<'a> {
    pub fn new(grammar: &'a Grammar) -> LALRParsingTables {
        LALRParsingTables { grammar }
    }

    pub fn compute_tables(&self) {
        let kernels = LR0Items::new(self.grammar).compute_kernels();
        let mut lookaheads_table = LookaheadsTable::new(&kernels);
        self.make_first_pass_of_propagation_for_starting_item(&kernels, &mut lookaheads_table);
        lookaheads_table.make_repeated_passes_of_propagation(&kernels);
        lookaheads_table.print();
    }

    fn make_first_pass_of_propagation_for_starting_item(
        &self,
        kernels: &Vec<HashSet<Production>>,
        lookaheads_table: &mut LookaheadsTable
    ) {
        // contract
        let mut starting_item = self.grammar.get_start_item();
        starting_item.rhs.insert(0, DOT.clone());
        assert_eq!(kernels[0], HashSet::from([starting_item.clone()]));
        // --
        lookaheads_table.add_lookahead(&SetItem::new(starting_item, 0), &EOT);
        for kernel in kernels {
            for sym in &self.grammar.symbols {
                self.discover_propagated_and_spontaneous_lookaheads(lookaheads_table, kernel, sym);
            }
        }
    }


    // page 272 in the Dragon Book
    fn discover_propagated_and_spontaneous_lookaheads(
        &self,
        lookaheads_table: &mut LookaheadsTable,
        lr0_kernel: &HashSet<Production>,
        sym: &Symbol,
    )  {
        for kernel_item in lr0_kernel {
            let closure_set =
                self.closure_with_lookahead(&HashSet::from([ProductionWithLookahead::new(
                    kernel_item.clone(),
                    UNKNOWN.clone(),
                )]));
            let mut goto_items = HashSet::new();
            for item in &closure_set {
                let dot_idx = item.production.find_indexes_of_symbol_on_rhs(&DOT);
                assert_eq!(dot_idx.len(), 1);
                let dot_idx = dot_idx[0];

                if dot_idx < item.production.rhs.len() - 1
                    && &item.production.rhs[dot_idx + 1] == sym
                {
                    let mut goto_item = item.clone();
                    goto_item.production.rhs.swap(dot_idx, dot_idx + 1);
                    goto_items.insert(goto_item);
                }
            }

            if goto_items.is_empty() {
                return;
            }

            let mut kernel_items = HashSet::new();
            for item in &goto_items {
                kernel_items.insert(item.production.clone());
            }

            let mut kernel_set_idx = None;
            let mut source_kernel_set_idx = None;
            for (set_idx, table_el)  in lookaheads_table.table.iter().enumerate() {
                let mut kernel_set = HashSet::new();
                for key in table_el.keys() {
                    kernel_set.insert(key.clone());
                }
                if kernel_set == kernel_items {
                    kernel_set_idx = Some(set_idx);
                }
                if kernel_set == *lr0_kernel {
                    source_kernel_set_idx = Some(set_idx);
                } 
            }

            for goto_item in &goto_items {         
                if goto_item.lookahead != UNKNOWN.clone() {
                    lookaheads_table.add_lookahead(
                        &SetItem::new(goto_item.production.clone(), kernel_set_idx.unwrap()), 
                        &goto_item.lookahead);
                } else {
                    lookaheads_table.add_propagation(
                        &SetItem::new(kernel_item.clone(), source_kernel_set_idx.unwrap()),
                         &SetItem::new(goto_item.production.clone(), kernel_set_idx.unwrap()));
                }
            }
                
        }
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
