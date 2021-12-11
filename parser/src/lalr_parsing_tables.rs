use crate::lookaheads_table::{LookaheadsTable, SetItem};
use crate::grammar::{Grammar, Production, ProductionWithLookahead, Symbol, DOT, EOT};
use crate::lr0_items::LR0Items;
use std::collections::{HashSet, HashMap};
use std::hash::Hash;
use std::{mem, panic, vec, fmt};

use prettytable::{Table, Row, Cell};

pub struct CanonicalLALRSets {
    sets: Vec<HashSet<ProductionWithLookahead>>
}
impl CanonicalLALRSets {
    pub fn get_n_sets(&self) -> usize {
        self.sets.len()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Action {
    Shift(usize),
    Reduce(Production),
    Accept,
    Error,
}

pub struct LALRParsingTables {
    action: Vec<Vec<Action>>,
    goto: Vec<Vec<usize>>,
    symbol_to_idx: HashMap<Symbol, usize>,
    terminals: HashSet<Symbol>, 
    nonterminals: HashSet<Symbol>
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
            action: vec![vec![Action::Error; terminals.len()]; no_sets],
            goto: vec![vec![usize::MAX; nonterminals.len()]; no_sets],
            symbol_to_idx,
            terminals,
            nonterminals,
        }
    }

    pub fn set_action(&mut self, set_idx: usize, terminal: &Symbol, act: Action) {
        if self.action[set_idx][*self.symbol_to_idx.get(terminal).unwrap()] == Action::Error {
            self.action[set_idx][*self.symbol_to_idx.get(terminal).unwrap()] = act;
        } else if self.action[set_idx][*self.symbol_to_idx.get(terminal).unwrap()] == act {
            return;
        } else {
            panic!("CONFLICT IN ACTION, {:?}, {:?}", self.action[set_idx][*self.symbol_to_idx.get(terminal).unwrap()], act);
        }
    }

    pub fn set_goto(&mut self, from_state: usize, nonterminal: &Symbol, to_state: usize) {
        if self.goto[from_state][*self.symbol_to_idx.get(nonterminal).unwrap()] == usize::MAX {
            self.goto[from_state][*self.symbol_to_idx.get(nonterminal).unwrap()] = to_state;
        } else if self.goto[from_state][*self.symbol_to_idx.get(nonterminal).unwrap()] == to_state {
            return;
        } else {
            panic!("CONFLICT IN GOTO, GRAMMAR IS NOT LALR(1)");
        }
        
    }

    pub fn print(&self) {
        println!("");
        println!("ACTION");
        let mut action_table = Table::new();
        let mut labels_row = vec![Cell::new(""); self.nonterminals.len() + self.terminals.len() + 1];
        for (sym, idx) in  &self.symbol_to_idx {
            if self.terminals.contains(sym) {
                labels_row[*idx + 1] = Cell::new(sym.clone().id.as_str());
            } else if self.nonterminals.contains(sym) {
                labels_row[*idx + self.nonterminals.len() + 1] = Cell::new(sym.clone().id.as_str());
            } else {
                unreachable!();
            }
        }

        action_table.add_row(Row::new(labels_row));
        for (state_idx, state) in self.action.iter().enumerate() {
            let mut row_vec = vec![Cell::new(state_idx.to_string().as_str())];
            for action in state {          
                match action {
                    Action::Shift(s) => row_vec.push(Cell::new(format!("s{}", s).as_str())),
                    Action::Reduce(r) => row_vec.push(Cell::new(format!("r{}", r).as_str())),
                    Action::Accept => row_vec.push(Cell::new(format!("acc").as_str())),
                    Action::Error => row_vec.push(Cell::new(format!(" ").as_str())),
                }
            }
            for goto_state in &self.goto[state_idx] {
                if *goto_state == usize::MAX {
                    row_vec.push(Cell::new(" "));
                } else {
                    row_vec.push(Cell::new(goto_state.to_string().as_str()))
                }
            }
            action_table.add_row(Row::new(row_vec));
        }
        action_table.printstd();
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
        LALRParsingTablesGenerator::compute_parsing_tables(grammar, canonical_lalr_sets);
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
        
        let mut lalr_tables = LALRParsingTables::new(
            sets.get_n_sets(), terminals, nonterminals.clone());
        LALRParsingTablesGenerator::populate_action(
            grammar,
            &sets,
            &mut lalr_tables);
        LALRParsingTablesGenerator::populate_goto(
            grammar,
            &sets,
            &mut lalr_tables, 
            &nonterminals);

        lalr_tables.print();
    }

    fn populate_goto(
        grammar: &Grammar,
        sets: &CanonicalLALRSets,
        lalr_tables: &mut LALRParsingTables,
        nonterminals: &HashSet<Symbol>,
    ) {
        for (set_idx, set) in sets.sets.iter().enumerate() {
            for sym in nonterminals {
                let goto_set = 
                            grammar.goto_with_lookahead(set, sym);
                for (goto_set_idx, set) in sets.sets.iter().enumerate() {
                    if goto_set == *set {
                        lalr_tables.set_goto(set_idx, sym, goto_set_idx);
                        break;
                    }
                }
            }
        }
    }

    fn populate_action(
        grammar: &Grammar,
        sets: &CanonicalLALRSets,
        lalr_tables: &mut LALRParsingTables
    ) {
        let mut accept_item = grammar.get_start_item();
        accept_item.rhs.push(DOT.clone());
        for (set_idx, set) in sets.sets.iter().enumerate() {
            for item in set {
                if item.production == accept_item {
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
                        // println!("");
                        // println!("set:");
                        // for p in set {
                        //     println!("{}", p);
                        // }
                        // println!("goto set: {}", sym_after_dot);
                        // for p in goto_set {
                        //     println!("{}", p);
                        // }
                    }
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

        for (idx, set) in sets.iter().enumerate() {
            println!("SET {}:", idx);
            for p in set {
                println!("{}", p);
            }
        }

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
