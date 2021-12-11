use crate::lookaheads_table::{LookaheadsTable, SetItem};
use crate::grammar::{Grammar, Production, ProductionWithLookahead, Symbol, DOT, EOT};
use crate::lr0_items::LR0Items;
use std::collections::{HashSet, HashMap};
use std::mem;

pub struct CanonicalLALRSets {
    sets: Vec<HashSet<ProductionWithLookahead>>
}

enum Action {
    Shift(usize),
    Reduce(Production),
    Accept,
}

pub struct LALRParsingTables {
    action: Vec<Vec<Action>>,
    goto: Vec<Vec<usize>>,
    symbol_to_idx: HashMap<Symbol, usize>,
}

pub struct LALRParsingTablesGenerator {  
}
impl LALRParsingTablesGenerator {

    pub fn compute_tables(grammar: &Grammar) {
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

    fn 

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
