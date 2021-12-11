use crate::grammar::{Grammar, Production, Symbol, DOT};
use std::collections::HashSet;

pub struct LR0Items<'a> {
    grammar: &'a Grammar,
}

impl<'a> LR0Items<'a> {
    pub fn new(grammar: &'a Grammar) -> LR0Items {
        LR0Items { grammar }
    }

    pub fn compute_kernels(&self) -> Vec<HashSet<Production>> {
        let mut sets_of_items = self.sets_of_lr0_items();
        for set in &mut sets_of_items {
            for p in set.clone().iter() {
                if !self.grammar.is_kernel_item(&p) {
                    set.remove(&p);
                }
            }
        }

        let mut empty_idxs = Vec::new();
        for (idx, set) in sets_of_items.iter().enumerate() {
            if set.is_empty() {
                empty_idxs.push(idx);
            }
        }
        for idx in empty_idxs {
            sets_of_items.swap_remove(idx);
        }

        sets_of_items
    }

    // Page 246 in the Dragon Book
    fn sets_of_lr0_items(&self) -> Vec<HashSet<Production>> {
        let mut sets_of_items = Vec::new();
        let mut initial_item = self.grammar.productions[0].clone();
        initial_item.rhs.insert(0, DOT.clone());
        let item_set = HashSet::from([initial_item]);

        sets_of_items.push(self.closure(&item_set));
        let mut new_sets_of_items = Vec::new();
        let mut new_set_of_items_added;

        loop {
            new_set_of_items_added = false;
            for set in &sets_of_items {
                for sym in &self.grammar.symbols {
                    let goto_set = self.goto(set, sym);
                    if !goto_set.is_empty()
                        && !sets_of_items.contains(&goto_set)
                        && !new_sets_of_items.contains(&goto_set)
                    {
                        new_sets_of_items.push(goto_set);
                        new_set_of_items_added = true;
                    }
                }
            }

            sets_of_items.extend(new_sets_of_items);
            new_sets_of_items = Vec::new();
            if !new_set_of_items_added {
                break;
            }
        }
        sets_of_items
    }

    // Page 245 in the Dragon Book
    fn closure(&self, items: &HashSet<Production>) -> HashSet<Production> {
        let mut set_of_items: HashSet<Production> = items.clone().into_iter().collect();
        let mut new_set_of_items = set_of_items.clone();
        let mut new_items_added;

        loop {
            new_items_added = false;
            for item in &set_of_items {
                let dot_idx = item.find_indexes_of_symbol_on_rhs(&*DOT);
                assert_eq!(dot_idx.len(), 1);
                let dot_idx = dot_idx[0];
                assert!(dot_idx < item.rhs.len());

                if dot_idx == item.rhs.len() - 1 {
                    continue;
                }

                let sym_after_dot = &item.rhs[dot_idx + 1];

                if self.grammar.is_terminal(sym_after_dot) {
                    continue;
                }
                for mut prod in self.grammar.get_productions_of(sym_after_dot) {
                    prod.rhs.insert(0, DOT.clone());
                    if !set_of_items.contains(&prod) && new_set_of_items.insert(prod) {
                        new_items_added = true;
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

    // Page 246 in the Dragon Book
    fn goto(&self, set_of_items: &HashSet<Production>, symbol: &Symbol) -> HashSet<Production> {
        let mut new_set = HashSet::new();
        for item in set_of_items {
            match self.move_dot_through(item, symbol) {
                Some(i) => {
                    new_set.insert(i);
                }
                None => (),
            };
        }

        self.closure(&new_set)
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
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils;
    #[test]
    fn closure_test() {
        let descript = r#"
            T -> program
            program -> 
            program -> program decl
            decl -> varDecl
            decl -> constDecl
            decl -> stmt
            "#
        .to_string();
        let grammar = Grammar::new(descript);
        let mut starting_item = grammar.productions[0].clone();
        starting_item.rhs.insert(0, DOT.clone());
        let item_set = HashSet::from([starting_item]);
        let closure = LR0Items::new(&grammar).closure(&item_set);

        let expected = HashSet::from([
            Production::from("T -> . program"),
            Production::from("program -> ."),
            Production::from("program -> . program decl"),
        ]);

        assert_eq!(closure, expected);
    }

    #[test]
    fn closure_test_2() {
        let descript = test_utils::get_test_grammar_1_description();

        let grammar = Grammar::new(descript);
        let mut starting_item = grammar.productions[0].clone();
        starting_item.rhs.insert(0, DOT.clone());
        let item_set = HashSet::from([starting_item]);

        let closure = LR0Items::new(&grammar).closure(&item_set);

        let expected = HashSet::from([
            Production::from("T -> . S"),
            Production::from("S -> . A S'"),
            Production::from("A -> . B A'"),
            Production::from("B -> . c"),
            Production::from("B -> . LP S RP"),
        ]);
        assert_eq!(closure, expected);
    }

    #[test]
    fn goto_test() {
        let descript = test_utils::get_test_grammar_3_description();

        let grammar = Grammar::new(descript);

        let mut starting_item = grammar.productions[0].clone();
        starting_item.rhs.insert(0, DOT.clone());
        let item_set = HashSet::from([starting_item]);
        let closure = LR0Items::new(&grammar).closure(&item_set);

        let value = LR0Items::new(&grammar).goto(&closure, &Symbol::new("C"));

        // page 262
        let expected = HashSet::from([
            Production::from("S -> C . C"),
            Production::from("C -> . c C"),
            Production::from("C -> . d"),
        ]);

        assert_eq!(value, expected);
    }

    #[test]
    fn lr0_item_sets_test() {
        let descript = test_utils::get_test_grammar_2_description();

        let grammar = Grammar::new(descript);

        let mut starting_item = grammar.productions[0].clone();
        starting_item.rhs.insert(0, DOT.clone());
        let item_set = HashSet::from([starting_item]);

        let lr0_sets = LR0Items::new(&grammar).sets_of_lr0_items();

        // page 255
        let expected: Vec<HashSet<Production>> = Vec::from([
            HashSet::from([
                Production::from("S' -> . S"),
                Production::from("S -> . L Eq R"),
                Production::from("S -> . R"),
                Production::from("L -> . Star R"),
                Production::from("L -> . id"),
                Production::from("R -> . L"),
            ]),
            HashSet::from([Production::from("S' -> S .")]),
            HashSet::from([
                Production::from("S -> L . Eq R"),
                Production::from("R -> L ."),
            ]),
            HashSet::from([Production::from("S -> R .")]),
            HashSet::from([
                Production::from("L -> Star . R"),
                Production::from("R -> . L"),
                Production::from("L -> . Star R"),
                Production::from("L -> . id"),
            ]),
            HashSet::from([Production::from("L -> id .")]),
            HashSet::from([
                Production::from("S -> L Eq . R"),
                Production::from("R -> . L"),
                Production::from("L -> . Star R"),
                Production::from("L -> . id"),
            ]),
            HashSet::from([Production::from("L -> Star R .")]),
            HashSet::from([Production::from("R -> L .")]),
            HashSet::from([Production::from("S -> L Eq R .")]),
        ]);

        test_utils::assert_vec_of_sets_of_items_eq(&lr0_sets, &expected)
    }

    #[test]
    fn kernels_of_lr0_items_test() {
        let descript = test_utils::get_test_grammar_2_description();

        let grammar = Grammar::new(descript);

        let mut starting_item = grammar.productions[0].clone();
        starting_item.rhs.insert(0, DOT.clone());
        let item_set = HashSet::from([starting_item]);

        let kernels = LR0Items::new(&grammar).compute_kernels();

        let expected: Vec<HashSet<Production>> = Vec::from([
            HashSet::from([Production::from("S' -> . S")]),
            HashSet::from([Production::from("S' -> S .")]),
            HashSet::from([
                Production::from("S -> L . Eq R"),
                Production::from("R -> L ."),
            ]),
            HashSet::from([Production::from("S -> R .")]),
            HashSet::from([Production::from("L -> Star . R")]),
            HashSet::from([Production::from("L -> id .")]),
            HashSet::from([Production::from("S -> L Eq . R")]),
            HashSet::from([Production::from("L -> Star R .")]),
            HashSet::from([Production::from("R -> L .")]),
            HashSet::from([Production::from("S -> L Eq R .")]),
        ]);

        test_utils::assert_vec_of_sets_of_items_eq(&kernels, &expected)
    }
}
