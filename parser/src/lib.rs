mod grammar;
use grammar::{Grammar, Symbol, Production, EPSILON, EOT};
use lazy_static::lazy_static;
use std::{collections::{HashSet, HashMap}, hash::Hash, iter::Product, ops::Index};
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
    fn sets_of_lr0_items(grammar: &Grammar) {
        const MAX_STATE_NUM: usize = 2000;
        let mut sets_of_items = Vec::new();
        let mut initial_item = grammar.productions[0].clone();
        initial_item.rhs.insert(0, DOT.clone());
        sets_of_items.push(Parser::closure(&grammar, &vec![initial_item]));
        
        let goto = Parser::make_goto_table(grammar, MAX_STATE_NUM);
        
        loop {
            for i in  [sets_of_items.len()] {
                for sym in &grammar.symbols {
                    match goto.get(sym) {
                        Some(states) => {
                            if states[]
                        },
                        None => (),
                    }
                }
            }
            break;
        }
    }

    fn goto(set_of_items: &HashSet<Production>, symbol: &Symbol) -> HashSet<Production> {

    }

    fn make_goto_table(grammar: &Grammar, state_num: usize) -> HashMap<Symbol, Vec<i32>>{
        
        let mut goto = HashMap::new();
        for sym in &grammar.symbols {
            goto.insert(sym.clone(), vec![0; state_num]);
        }
        goto
    } 

    // Page 261 in the Dragon Book
    fn closure(grammar: &Grammar, items: &Vec<Production>) -> HashSet<Production> {
        let mut set_of_items: HashSet<Production> = items.clone().into_iter().collect();
        let mut new_set_of_items = set_of_items.clone();
        let mut new_items_added;

        loop {
            new_items_added = false;
            for item in &set_of_items {
                let mut sym_after_dot = &*EPSILON;
                let mut prev_rhs_sym = &*EPSILON;
                for rhs_sym in &item.rhs {
                    if *prev_rhs_sym == *DOT {
                        sym_after_dot = rhs_sym;
                    }
                    prev_rhs_sym = rhs_sym;
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
    use std::str::ParseBoolError;

    use super::*;

    #[test]
    fn closure_test() {
        let descript = 
            r#"
            T -> program
            program -> epsilon
            program -> program decl
            decl -> varDecl
            decl -> constDecl
            decl -> stmt
            "#.to_string();
        let g = Grammar::new(descript);
        let p = Parser::new(g);
        let mut starting_item = p.grammar.productions[0].clone();
        starting_item.rhs.insert(0, DOT.clone());
        let closure = p.closure(&vec![starting_item]);

        let t =  Symbol::new("T");
        let program = Symbol::new("program");
        let decl = Symbol::new("decl");
        let var_decl = Symbol::new("varDecl");
        let const_decl = Symbol::new("constDecl");
        let statement = Symbol::new("statement");
        let epsilon = Symbol::new("epsilon");

        let expected = HashSet::from([
            Production{lhs: t.clone(), rhs: vec![DOT.clone(), program.clone()]},
            Production{lhs: program.clone(), rhs: vec![DOT.clone(), EPSILON.clone()]},
            Production{lhs: program.clone(), rhs: vec![DOT.clone(), program.clone(), decl.clone()]},
        ]);

        assert_eq!(closure, expected);
    }

    #[test]
    fn closure_test_2() {
        let descript = 
            r#"
            T -> S EOT
            S -> A Sp
            Sp -> Plus A Sp
            Sp -> epsilon
            A -> B Ap
            Ap -> Star B Ap
            Ap -> epsilon
            B -> LP S RP
            B -> c
            "#.to_string();

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

        let g = Grammar::new(descript);
        let p = Parser::new(g);
        let mut starting_item = p.grammar.productions[0].clone();
        starting_item.rhs.insert(0, DOT.clone());
        let closure = p.closure(&vec![starting_item]);
        let expected = HashSet::from([
            Production{lhs: t.clone(), rhs: vec![DOT.clone(), s.clone(), EOT.clone()]},
            Production{lhs: s.clone(), rhs: vec![DOT.clone(), a.clone(), sp.clone()]},
            Production{lhs: a.clone(), rhs: vec![DOT.clone(), b.clone(), ap.clone()]},
            Production{lhs: b.clone(), rhs: vec![DOT.clone(), c.clone()]},
            Production{lhs: b.clone(), rhs: vec![DOT.clone(), lp.clone(), s.clone(), rp.clone()]},
        ]);
    }

}







