use crate::lookaheads_table::{LookaheadsTable, SetItem};
use crate::grammar::{Grammar, Production, ProductionWithLookahead, Symbol, DOT, EOT};
use crate::lr0_items::LR0Items;
use std::collections::{HashSet, HashMap};
use std::hash::Hash;
use std::{mem, panic};

pub struct CanonicalLALRSets {
    sets: Vec<HashSet<ProductionWithLookahead>>
}
impl CanonicalLALRSets {
    pub fn get_n_sets(&self) -> usize {
        self.sets.len()
    }
}

#[derive(Clone, PartialEq, Eq)]
pub enum Action {
    Shift(usize),
    Reduce(Production),
    Accept,
    Empty,
}

pub struct LALRParsingTables {
    action: Vec<Vec<Action>>,
    goto: Vec<Vec<usize>>,
    symbol_to_idx: HashMap<Symbol, usize>,
}
impl LALRParsingTables {
    pub fn new(no_sets: usize, terminals: HashSet<Symbol>, nonterminals: HashSet<Symbol>) -> LALRParsingTables {
        let mut symbol_to_idx = HashMap::new();
        for (idx, sym) in terminals.iter().enumerate() {
            symbol_to_idx.insert(sym.clone(), idx);
        }
        for (idx, sym) in  nonterminals.iter().enumerate() {
            symbol_to_idx.insert(sym.clone(), idx);
        }
        LALRParsingTables {
            action: vec![vec![Action::Empty; terminals.len()]; no_sets],
            goto: vec![vec![usize::MAX; nonterminals.len()]; no_sets],
            symbol_to_idx,
        }
    }

    pub fn set_action(&mut self, set_idx: usize, terminal: &Symbol, act: Action) {
        if self.action[set_idx][*self.symbol_to_idx.get(terminal).unwrap()] == Action::Empty {
            self.action[set_idx][*self.symbol_to_idx.get(terminal).unwrap()] = act;
        } else {
            panic!("GRAMMAR IS NOT LALR(1)")
        }
    }

    pub fn set_goto(&mut self, from_state: usize, nonterminal: &Symbol, to_state: usize) {
        self.goto[from_state][*self.symbol_to_idx.get(nonterminal).unwrap()] = to_state;
    }
}

pub struct LALRParsingTablesGenerator {  
}
impl LALRParsingTablesGenerator {

    pub fn compute(grammar: &Grammar) {
        let kernels = LR0Items::new(grammar).compute_kernels();
        let mut lookaheads_table = LookaheadsTable::new(grammar, kernels);
        lookaheads_table.compute();
        lookaheads_table.print();
        let canonical_lalr_sets = 
            LALRParsingTablesGenerator::convert_lookaheads_table_to_canonical_lalr_sets(
                &lookaheads_table,
                grammar,
            );
    }

    fn compute_parsing_tables(grammar: &Grammar, sets: CanonicalLALRSets) {
        let mut terminals = HashSet::new();
        let mut nonterminals = HashSet::new();
        let symbols = grammar.symbols.clone();

        terminals.insert(EOT.clone());
        for sym in symbols {
            if grammar.is_terminal(&sym) {
                terminals.insert(sym);
            } else {
                nonterminals.insert(sym);
            }
        }
        
        let mut lalr_tables = LALRParsingTables::new(sets.get_n_sets(), terminals, nonterminals);
        let mut accept_item = grammar.get_start_item();
        accept_item.rhs.push(DOT.clone());
        for (set_idx, set) in sets.sets.iter().enumerate() {
            LALRParsingTablesGenerator::populate_action(
                grammar,
                &sets,
                set,
                set_idx,
                &accept_item,
                &mut lalr_tables,
            )
        }
    }

    fn populate_goto(
        grammar: &Grammar,
        sets: &CanonicalLALRSets,
        lalr_tables: &mut LALRParsingTables
    ) {
        for (set_idx, set) in sets.sets.iter().enumerate() {
            
        }
    }

    fn populate_action(
        grammar: &Grammar,
        sets: &CanonicalLALRSets,
        set: &HashSet<ProductionWithLookahead>,
        set_idx: usize,
        accept_item: &Production, 
        lalr_tables: &mut LALRParsingTables
    ) {
        for item in set {
            if item.production == *accept_item {
                lalr_tables.set_action(set_idx, &EOT, Action::Accept)
            } 
            else if item.production.rhs.last().unwrap() == &DOT.clone() {
                lalr_tables.set_action(set_idx, &item.lookahead, Action::Reduce(item.production.clone()))
            }
            else {
                let dot_idx = item.production.find_indexes_of_symbol_on_rhs(&DOT);
                assert_eq!(dot_idx.len(), 1);
                let dot_idx = dot_idx[0];
                assert!(dot_idx < item.production.rhs.len() - 1);
                let sym_after_dot = &item.production.rhs[dot_idx + 1];
                if grammar.is_terminal(sym_after_dot) {
                    let goto_set = 
                        grammar.goto_with_lookahead(set, sym_after_dot);
                    let mut success = false;
                    for (shift_set_idx, set) in sets.sets.iter().enumerate() {
                        if goto_set == *set {
                            lalr_tables.set_action(set_idx, sym_after_dot, Action::Shift(shift_set_idx));
                            success = true;
                            break;
                        }
                    }
                    assert!(success);
                }
            }
        }
    }

    fn convert_lookaheads_table_to_canonical_lalr_sets(
        lookaheads_table: &LookaheadsTable,
        grammar: &Grammar,
    ) -> CanonicalLALRSets {
        let mut sets = Vec::new();
        for table_el in &lookaheads_table.table {
            let mut kernel_with_lookaheads = HashSet::new();
            for (kernel_item, lookaheads) in table_el {
                for lookahead in lookaheads {
                    kernel_with_lookaheads.insert(
                        ProductionWithLookahead::new(kernel_item.clone(), lookahead.clone())
                    );
                }
            }
            sets.push(grammar.closure_with_lookahead(&kernel_with_lookaheads));
        }

        // for (idx, set) in sets.iter().enumerate() {
        //     println!("SET {}:", idx);
        //     for p in set {
        //         println!("{}", p);
        //     }
        // }

        CanonicalLALRSets {
            sets
        }
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
    }

 
}
