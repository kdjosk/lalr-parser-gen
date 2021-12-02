mod grammar;
use grammar::{Grammar, Symbol, Production, EOT};
use lazy_static::lazy_static;
use std::{collections::{HashSet, HashMap}};
struct Parser {
    grammar: Grammar,
    sets_of_items: Vec<HashSet<Production>>,
}

lazy_static! {
    static ref DOT: Symbol = Symbol::new(".");
}

impl Parser {

    pub fn new(grammar: Grammar) -> Parser {


        Parser { 
            grammar,
            sets_of_items: Vec::new(),
        }
    }

    // Page 246 in the Dragon Book
    fn sets_of_lr0_items(grammar: &Grammar) -> Vec<HashSet<Production>>{
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
                    if !goto_set.is_empty() && !sets_of_items.contains(&goto_set) && !new_sets_of_items.contains(&goto_set) {
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
    fn goto(grammar: &Grammar, set_of_items: &HashSet<Production>, symbol: &Symbol) -> HashSet<Production> {
        let mut new_set = HashSet::new();
        for item in set_of_items {
            match Parser::move_dot_through(item, symbol) {
                Some(i) => {
                    new_set.insert(i);
                },
                None => (),
            };
        }

        Parser::closure(grammar, &new_set)
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
                return Some(new_prod)
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
                    if !set_of_items.contains(&prod) && new_set_of_items.insert(prod){
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

}

#[cfg(test)]
mod tests {
    use std::{str::ParseBoolError, vec};

    use super::*;

    #[test]
    fn closure_test() {
        let descript = 
            r#"
            T -> program
            program -> 
            program -> program decl
            decl -> varDecl
            decl -> constDecl
            decl -> stmt
            "#.to_string();
        let grammar = Grammar::new(descript);
        let mut starting_item = grammar.productions[0].clone();
        starting_item.rhs.insert(0, DOT.clone());
        let item_set = HashSet::from([starting_item]);
        let closure = Parser::closure(&grammar, &item_set);

        let t =  Symbol::new("T");
        let program = Symbol::new("program");
        let decl = Symbol::new("decl");
        let var_decl = Symbol::new("varDecl");
        let const_decl = Symbol::new("constDecl");
        let statement = Symbol::new("statement");

        let expected = HashSet::from([
            Production{lhs: t.clone(), rhs: vec![DOT.clone(), program.clone()]},
            Production{lhs: program.clone(), rhs: vec![DOT.clone()]},
            Production{lhs: program.clone(), rhs: vec![DOT.clone(), program.clone(), decl.clone()]},
        ]);

        assert_eq!(closure, expected);
    }

    #[test]
    fn closure_test_2() {
        let descript = get_test_grammar_1_description();

        let t =  Symbol::new("T");
        let s =  Symbol::new("S");
        let a = Symbol::new("A");
        let sp = Symbol::new("Sp");
        let ap = Symbol::new("Ap");
        let b = Symbol::new("B");
        let lp = Symbol::new("LP");
        let rp = Symbol::new("RP");
        let c = Symbol::new("c");
        let star = Symbol::new("Star");
        let plus = Symbol::new("Plus");
        let epsilon = Symbol::new("epsilon");

        let grammar = Grammar::new(descript);
        let mut starting_item = grammar.productions[0].clone();
        starting_item.rhs.insert(0, DOT.clone());
        let item_set = HashSet::from([starting_item]);

        let closure = Parser::closure(&grammar, &item_set);
        let expected = HashSet::from([
            Production{lhs: t.clone(), rhs: vec![DOT.clone(), s.clone(), EOT.clone()]},
            Production{lhs: s.clone(), rhs: vec![DOT.clone(), a.clone(), sp.clone()]},
            Production{lhs: a.clone(), rhs: vec![DOT.clone(), b.clone(), ap.clone()]},
            Production{lhs: b.clone(), rhs: vec![DOT.clone(), c.clone()]},
            Production{lhs: b.clone(), rhs: vec![DOT.clone(), lp.clone(), s.clone(), rp.clone()]},
        ]);

        assert_eq!(closure, expected);
    }

    #[test]
    fn goto_test() {
        let descript = get_test_grammar_1_description();

        let t =  Symbol::new("T");
        let s =  Symbol::new("S");
        let a = Symbol::new("A");
        let sp = Symbol::new("Sp");
        let ap = Symbol::new("Ap");
        let b = Symbol::new("B");
        let lp = Symbol::new("LP");
        let rp = Symbol::new("RP");
        let c = Symbol::new("c");
        let star = Symbol::new("Star");
        let plus = Symbol::new("Plus");
        let epsilon = Symbol::new("epsilon");

        let grammar = Grammar::new(descript);

        let mut starting_item = grammar.productions[0].clone();
        starting_item.rhs.insert(0, DOT.clone());
        let item_set = HashSet::from([starting_item]);
        let closure = Parser::closure(&grammar, &item_set);

        let res = Parser::goto(&grammar, &closure, &a);

        let expected = Production{lhs: s.clone(), rhs: vec![a.clone(), DOT.clone(), sp.clone()]};
        assert!(res.contains(&expected));
    }

    #[test]
    fn lr0_item_sets_test() {
        let descript = get_test_grammar_1_description();

        let t =  Symbol::new("T");
        let s =  Symbol::new("S");
        let a = Symbol::new("A");
        let sp = Symbol::new("Sp");
        let ap = Symbol::new("Ap");
        let b = Symbol::new("B");
        let lp = Symbol::new("LP");
        let rp = Symbol::new("RP");
        let c = Symbol::new("c");
        let star = Symbol::new("Star");
        let plus = Symbol::new("Plus");
        let epsilon = Symbol::new("epsilon");

        let grammar = Grammar::new(descript);
   
        let mut starting_item = grammar.productions[0].clone();
        starting_item.rhs.insert(0, DOT.clone());
        let item_set = HashSet::from([starting_item]);

        let res = Parser::sets_of_lr0_items(&grammar);

        for (idx, s) in res.iter().enumerate() {
            println!("set nr. {}", idx);
            for p in s {
                println!("{}", p);
            }
        }
    }

    fn get_test_grammar_1_description() -> String {
        r#"
        T -> S EOT
        S -> A Sp
        Sp -> Plus A Sp
        Sp -> 
        A -> B Ap
        Ap -> Star B Ap
        Ap -> 
        B -> LP S RP
        B -> c
        "#.to_string()
    }
}







