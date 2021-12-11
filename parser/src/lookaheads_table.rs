use crate::grammar::{Grammar, Production, ProductionWithLookahead, Symbol, EOT, DOT};
use std::collections::{HashMap, HashSet};
use std::mem;
use lazy_static::lazy_static;

lazy_static! {
    static ref UNKNOWN: Symbol = Symbol::new("UNKNOWN");
}


#[derive(PartialEq, Eq, Hash, Clone)]
pub struct SetItem {
    production: Production,
    set_idx: usize,
}
impl SetItem {
    pub fn new(production: Production, set_idx: usize) -> SetItem {
        SetItem { production, set_idx }
    }
}

pub struct LookaheadsTable<'a> {
    grammar: &'a Grammar,
    lr0_kernels: Vec<HashSet<Production>>,
    pub table: Vec<HashMap<Production, HashSet<Symbol>>>,
    table_swap_copy: Vec<HashMap<Production, HashSet<Symbol>>>,
    propagation: HashMap<SetItem, HashSet<SetItem>>, 
}
impl<'a> LookaheadsTable<'a> {
    pub fn new(grammar: &'a Grammar, lr0_kernels: Vec<HashSet<Production>>) -> LookaheadsTable<'a> {
        let mut table = Vec::new();
        let mut item_to_set_idx = HashMap::new();
        let mut propagation = HashMap::new();
        for (idx, k) in lr0_kernels.iter().enumerate() {
            let mut table_el = HashMap::new();
            for p in k {
                table_el.insert(p.clone(), HashSet::new());
                item_to_set_idx.insert(p.clone(), idx);
                propagation.insert(SetItem::new(p.clone(), idx), HashSet::new());
            }
            table.push(table_el);
        }
        LookaheadsTable {
            grammar,
            lr0_kernels,
            table_swap_copy: table.clone(),
            table,
            propagation,
        }     
    }

    pub fn compute(&mut self) {
        self.init_propagation_rules_and_spontaneous_lookaheads();
        self.make_repeated_passes_of_propagation()
    }
    
    pub fn add_lookahead(&mut self, item: &SetItem, lookahead: &Symbol) {
        let current_lookaheads = self.table[item.set_idx].get_mut(&item.production).unwrap();
        current_lookaheads.insert(lookahead.clone());
    }

    fn init_propagation_rules_and_spontaneous_lookaheads(
        &mut self,
    ) {
        // contract
        let mut starting_item = self.grammar.get_start_item();
        starting_item.rhs.insert(0, DOT.clone());
        assert_eq!(self.lr0_kernels[0], HashSet::from([starting_item.clone()]));
        // --
        self.add_lookahead(&SetItem::new(starting_item, 0), &EOT);
        for kernel in self.lr0_kernels.clone() {
            for sym in &self.grammar.symbols {
                self.discover_propagated_and_spontaneous_lookaheads(&kernel, sym);
            }
        }
    }

    pub fn make_repeated_passes_of_propagation(
        &mut self,
    ) {
        loop {
            for (set_idx, kernel) in self.lr0_kernels.clone().iter().enumerate() {
                for kernel_item in kernel {
                    self.propagate_lookaheads(&SetItem::new(kernel_item.clone(), set_idx))
                }
            }
            if self.table == self.table_swap_copy {
                break;
            }
            mem::swap(&mut self.table, &mut self.table_swap_copy);
        } 
    }

    // TODO - Gets stuck on pass 1 state
    pub fn propagate_lookaheads(&mut self, from: &SetItem) {
        let from_lookaheads = self.table[from.set_idx].get(&from.production).unwrap().clone();
        self.table_swap_copy[from.set_idx].get_mut(&from.production).unwrap().extend(from_lookaheads.clone());
        let propagations = self.propagation.get(from).unwrap();

        for item in propagations {     
            let item_lookaheads = self.table_swap_copy[item.set_idx].get_mut(&item.production).unwrap();
            if *item_lookaheads != from_lookaheads {
                item_lookaheads.extend(from_lookaheads.clone())
            }     
        }
    }

    pub fn add_propagation(&mut self, from: &SetItem, to: &SetItem) {
        let targets = self.propagation.get_mut(from).unwrap();
        targets.insert(to.clone());
    }

    // page 272 in the Dragon Book
    fn discover_propagated_and_spontaneous_lookaheads(
        &mut self,
        lr0_kernel: &HashSet<Production>,
        sym: &Symbol,
    )  {
        for kernel_item in lr0_kernel {
            let closure_set =
                self.grammar.closure_with_lookahead(&HashSet::from([ProductionWithLookahead::new(
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
                continue;
            }

            let mut kernel_items = HashSet::new();
            for item in &goto_items {
                kernel_items.insert(item.production.clone());
            }

            let mut kernel_set_idx = None;
            let mut source_kernel_set_idx = None;
            for (set_idx, table_el)  in self.table.iter().enumerate() {
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
                    self.add_lookahead(
                        &SetItem::new(goto_item.production.clone(), kernel_set_idx.unwrap()), 
                        &goto_item.lookahead);
                } else {
                    self.add_propagation(
                        &SetItem::new(kernel_item.clone(), source_kernel_set_idx.unwrap()),
                            &SetItem::new(goto_item.production.clone(), kernel_set_idx.unwrap()));
                }
            }
                
        }
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils;
    use crate::lr0_items::LR0Items;

}