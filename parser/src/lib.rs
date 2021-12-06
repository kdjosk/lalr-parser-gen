pub mod grammar;
use grammar::{Grammar, Production, ProductionWithLookahead, Symbol, EOT, DOT};
use lazy_static::lazy_static;
use std::collections::HashSet;
pub struct Parser {

}

lazy_static! {
    static ref UNKNOWN: Symbol = Symbol::new("UNKNOWN");
}

impl Parser {
    pub fn new(grammar: Grammar) -> Parser {
        let kernels = Parser::kernels_of_lr0_items(&grammar);
        for sym in &grammar.symbols {
            let propagated_spontaneous = 
                Parser::discover_propagated_and_spontaneous_lookaheads(&grammar, &kernels[0], sym);
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
        
        Parser {
        }
    }

    fn kernels_of_lr0_items(grammar: &Grammar) -> Vec<HashSet<Production>> {
        let mut sets_of_items = Parser::sets_of_lr0_items(grammar);
        for set in &mut sets_of_items {
            for p in set.clone().iter() {
                if !grammar.is_kernel_item(&p) {
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

    // page 272 in the Dragon Book
    fn discover_propagated_and_spontaneous_lookaheads(
        grammar: &Grammar,
        lr0_kernel: &HashSet<Production>,
        sym: &Symbol,
    ) -> Lookaheads {
        let mut lookaheads = Lookaheads::new();
        for item in lr0_kernel {
            let closure_set = Parser::closure_with_lookahead(
                grammar,
                &HashSet::from([ProductionWithLookahead::new(item.clone(), UNKNOWN.clone())]),
            );
            println!("Closure Set");
            for p in &closure_set {
                println!("{}", p);
            }
            for item in &closure_set {
                let dot_idx = item.production.find_indexes_of_symbol_on_rhs(&DOT);
                assert_eq!(dot_idx.len(), 1);
                let dot_idx = dot_idx[0];

                if dot_idx < item.production.rhs.len() - 1
                    && &item.production.rhs[dot_idx + 1] == sym
                {
                    let mut goto_item = item.clone();
                    goto_item.production.rhs.swap(dot_idx, dot_idx + 1);

                    if item.lookahead != UNKNOWN.clone()
                    {  
                        lookaheads.spontaneous.insert(goto_item);  
                    } else {
                        lookaheads.propagated.insert(goto_item);
                    }

                }
            }
        }
        lookaheads
    }
    

    // Page 246 in the Dragon Book
    fn sets_of_lr0_items(grammar: &Grammar) -> Vec<HashSet<Production>> {
        let mut sets_of_items = Vec::new();
        let mut initial_item = grammar.productions[0].clone();
        initial_item.rhs.insert(0, DOT.clone());
        let item_set = HashSet::from([initial_item]);

        sets_of_items.push(Parser::closure(&grammar, &item_set));
        let mut new_sets_of_items = Vec::new();
        let mut new_set_of_items_added;

        loop {
            new_set_of_items_added = false;
            for set in &sets_of_items {
                for sym in &grammar.symbols {
                    let goto_set = Parser::goto(grammar, set, sym);
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

    // Page 246 in the Dragon Book
    fn goto(
        grammar: &Grammar,
        set_of_items: &HashSet<Production>,
        symbol: &Symbol,
    ) -> HashSet<Production> {
        let mut new_set = HashSet::new();
        for item in set_of_items {
            match Parser::move_dot_through(item, symbol) {
                Some(i) => {
                    new_set.insert(i);
                }
                None => (),
            };
        }

        Parser::closure(grammar, &new_set)
    }

    // page 261
    fn goto_with_lookahead(
        grammar: &Grammar,
        set_of_items: &HashSet<ProductionWithLookahead>,
        symbol: &Symbol,
    ) -> HashSet<ProductionWithLookahead> {
        let mut new_set = HashSet::new();
        for item in set_of_items {
            match Parser::move_dot_through(&item.production, symbol) {
                Some(i) => {
                    new_set.insert(ProductionWithLookahead{
                        production: i,
                        lookahead: item.lookahead.clone(),
                    });
                }
                None => (),
            };
        }
        Parser::closure_with_lookahead(grammar, &new_set)
    }

    fn move_dot_through(item: &Production, symbol: &Symbol) -> Option<Production> {
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

    // Page 245 in the Dragon Book
    fn closure(grammar: &Grammar, items: &HashSet<Production>) -> HashSet<Production> {
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

                if grammar.is_terminal(sym_after_dot) {
                    continue;
                }
                for mut prod in grammar.get_productions_of(sym_after_dot) {
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

    fn closure_with_lookahead(
        grammar: &Grammar,
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
                if grammar.is_terminal(sym_after_dot) {
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

                for prod in grammar.get_productions_of(sym_after_dot) {
                    for terminal in grammar.first(&postfix) {
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


#[cfg(test)]
mod tests {
    use std::hash::Hash;

    use super::*;

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
        let closure = Parser::closure(&grammar, &item_set);

        let expected = HashSet::from([
            Production::from("T -> . program"),
            Production::from("program -> ."),
            Production::from("program -> . program decl"),
        ]);

        assert_eq!(closure, expected);
    }

    #[test]
    fn closure_test_2() {
        let descript = get_test_grammar_1_description();

        let grammar = Grammar::new(descript);
        let mut starting_item = grammar.productions[0].clone();
        starting_item.rhs.insert(0, DOT.clone());
        let item_set = HashSet::from([starting_item]);

        let closure = Parser::closure(&grammar, &item_set);

        let expected = HashSet::from([
            Production::from("T -> . S EOT"),
            Production::from("S -> . A S'"),
            Production::from("A -> . B A'"),
            Production::from("B -> . c"),
            Production::from("B -> . LP S RP"),
        ]);
        assert_eq!(closure, expected);
    }

    #[test]
    fn closure_with_lookahead_test() {
        let descript = get_test_grammar_3_description();

        let grammar = Grammar::new(descript);
        let mut starting_item = grammar.productions[0].clone();
        starting_item.rhs.insert(0, DOT.clone());
        let starting_item = ProductionWithLookahead::new( 
            starting_item,
            EOT.clone(),
        );
        let item_set = HashSet::from([starting_item]);

        let closure = Parser::closure_with_lookahead(&grammar, &item_set);

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
        let descript = get_test_grammar_3_description();
        let grammar = Grammar::new(descript);   
        let mut starting_item = grammar.productions[0].clone();
        starting_item.rhs.insert(0, DOT.clone());

        let mut starting_item = ProductionWithLookahead::new( 
            starting_item,
            EOT.clone(),
        );
        let starting_item_closure = 
            Parser::closure_with_lookahead(&grammar, &HashSet::from([starting_item]));
        let value = 
            Parser::goto_with_lookahead(
                &grammar, 
                &starting_item_closure,
                &Symbol::new("C")
            );
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

    #[test]
    fn goto_test() {
        let descript = get_test_grammar_3_description();

        let grammar = Grammar::new(descript);

        let mut starting_item = grammar.productions[0].clone();
        starting_item.rhs.insert(0, DOT.clone());
        let item_set = HashSet::from([starting_item]);
        let closure = Parser::closure(&grammar, &item_set);

        let value = Parser::goto(&grammar, &closure, &Symbol::new("C"));

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
        let descript = get_test_grammar_2_description();

        let grammar = Grammar::new(descript);

        let mut starting_item = grammar.productions[0].clone();
        starting_item.rhs.insert(0, DOT.clone());
        let item_set = HashSet::from([starting_item]);

        let lr0_sets = Parser::sets_of_lr0_items(&grammar);

        // page 255
        let expected: Vec<HashSet<Production>> = Vec::from([
            HashSet::from([
                Production::from("S' -> . S EOT"),
                Production::from("S -> . L Eq R"),
                Production::from("S -> . R"),
                Production::from("L -> . Star R"),
                Production::from("L -> . id"),
                Production::from("R -> . L"),
            ]),
            HashSet::from([
                Production::from("S' -> S . EOT"),
            ]),
            HashSet::from([
                Production::from("S -> L . Eq R"),
                Production::from("R -> L ."),
            ]),
            HashSet::from([
                Production::from("S -> R ."),
            ]),
            HashSet::from([
                Production::from("L -> Star . R"),
                Production::from("R -> . L"),
                Production::from("L -> . Star R"),
                Production::from("L -> . id"),
            ]),
            HashSet::from([
                Production::from("L -> id ."),
            ]),
            HashSet::from([
                Production::from("S -> L Eq . R"),
                Production::from("R -> . L"),
                Production::from("L -> . Star R"),
                Production::from("L -> . id"),
            ]),
            HashSet::from([
                Production::from("L -> Star R ."),
            ]),
            HashSet::from([
                Production::from("R -> L ."),
            ]),
            HashSet::from([
                  Production::from("S -> L Eq R ."),
            ]),
        ]);

        assert_vec_of_sets_of_items_eq(&lr0_sets, &expected)
    }

    #[test]
    fn kernels_of_lr0_items_test() {
        let descript = get_test_grammar_2_description();

        let grammar = Grammar::new(descript);

        let mut starting_item = grammar.productions[0].clone();
        starting_item.rhs.insert(0, DOT.clone());
        let item_set = HashSet::from([starting_item]);

        let kernels = Parser::kernels_of_lr0_items(&grammar);

        let expected: Vec<HashSet<Production>> = Vec::from([
            HashSet::from([
                Production::from("S' -> . S EOT"),
            ]),
            HashSet::from([
                Production::from("S' -> S . EOT"),
            ]),
            HashSet::from([
                Production::from("S -> L . Eq R"),
                Production::from("R -> L ."),
            ]),
            HashSet::from([
                Production::from("S -> R ."),
            ]),
            HashSet::from([
                Production::from("L -> Star . R"),
            ]),
            HashSet::from([
                Production::from("L -> id ."),
            ]),
            HashSet::from([
                Production::from("S -> L Eq . R"),
            ]),
            HashSet::from([
                Production::from("L -> Star R ."),
            ]),
            HashSet::from([
                Production::from("R -> L ."),
            ]),
            HashSet::from([
                  Production::from("S -> L Eq R ."),
            ]),
        ]);

        assert_vec_of_sets_of_items_eq(&kernels, &expected)
    }

    #[test]
    fn discover_propagated_and_spontaneous_lookaheads_test() {
        let grammar = get_test_grammar_2_description();
        let lr0_kernel = HashSet::from([
            ProductionWithLookahead::from("S' -> . S", "UNKNOWN"),
        ]);
        
        let prop_spont = 
            Parser::discover_propagated_and_spontaneous_lookaheads(grammar, lr0_kernel, sym);
    }

    fn assert_vec_of_sets_of_items_eq(
        v1: &Vec<HashSet<Production>>,
        v2: &Vec<HashSet<Production>>
    ) {
        assert_eq!(v1.len(), v2.len());
        for set in v1 {
            assert!(v2.contains(set), "No set {:?}", set);
        }
    }

    fn get_test_grammar_1_description() -> String {
        r#"
        T -> S EOT
        S -> A S'
        S' -> Plus A S'
        S' -> 
        A -> B A'
        A' -> Star B A'
        A' -> 
        B -> LP S RP
        B -> c
        "#
        .to_string()
    }

    fn get_test_grammar_2_description() -> String {
        r#"
        S' -> S EOT
        S -> L Eq R
        S -> R
        L -> Star R
        L -> id
        R -> L
        "#
        .to_string()
    }

    // page 263
    fn get_test_grammar_3_description() -> String {
        r#"
        S' -> S EOT
        S -> C C
        C -> c C
        C -> d
        "#
        .to_string()
    }
}
