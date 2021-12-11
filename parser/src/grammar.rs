use lazy_static::lazy_static;
use regex::Regex;
use std::collections::HashSet;

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Production {
    pub lhs: Symbol,
    pub rhs: Vec<Symbol>,
}

impl Production {
    pub fn from(description: &str) -> Production {
        let description = description.to_string();
        let mut token_string = String::new();
        let mut lhs = Symbol::new(String::new().as_str());
        let mut rhs = Vec::new();
        lazy_static! {
            static ref SYMBOL_REGEX: Regex = Regex::new(r"[a-zA-Z\.']").unwrap();
        }
        let description = description.trim();
        let chars: Vec<_> = description.chars().collect();
        for (idx, &c) in chars.iter().enumerate() {
            if SYMBOL_REGEX.is_match(c.to_string().as_str()) {
                token_string.push(c);
            }
            if c == '-' {
                if lhs.id.is_empty() || !rhs.is_empty() {
                    panic!("Unexpected character -");
                }
                if chars.len() > idx + 1 && chars[idx + 1] == '>' {
                    continue;
                } else {
                    panic!("Expected `>` after `-`");
                }
            }
            if c.is_whitespace() || idx == chars.len() - 1 {
                if lhs.id.is_empty() && !token_string.is_empty() {
                    lhs.id = token_string;
                    token_string = String::new();
                } else if !token_string.is_empty() {
                    rhs.push(Symbol::new(token_string.as_str()));
                    token_string = String::new();
                }
            }
        }
        if !(lhs.id.is_empty() && rhs.is_empty()) {
            return Production { lhs, rhs };
        } else {
            panic!("No LHS and RHS in production {}", description);
        }
    }

    pub fn find_indexes_of_symbol_on_rhs(&self, sym: &Symbol) -> Vec<usize> {
        let mut res = Vec::new();
        for (i, s) in self.rhs.iter().enumerate() {
            if s == sym {
                res.push(i);
            }
        }
        res
    }

    pub fn get_dot_index(&self) -> usize {
        let idx = self.find_indexes_of_symbol_on_rhs(&DOT);
        assert_eq!(idx.len(), 1);
        idx[0]
    }
}

impl Display for Production {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} -> ", self.lhs)?;
        for s in &self.rhs {
            write!(f, "{} ", s)?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ProductionWithLookahead {
    pub production: Production,
    pub lookahead: Symbol,
}

impl ProductionWithLookahead {
    pub fn new(production: Production, lookahead: Symbol) -> ProductionWithLookahead {
        ProductionWithLookahead {
            production,
            lookahead,
        }
    }
    pub fn from(description: &str, lookahead: &str) -> ProductionWithLookahead {
        ProductionWithLookahead {
            production: Production::from(description),
            lookahead: Symbol::new(lookahead),
        }
    }
}

impl Display for ProductionWithLookahead {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[{}, {}]", self.production, self.lookahead)?;
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Hash, Eq, PartialOrd, Ord)]
pub struct Symbol {
    id: String,
}

use core::fmt;
use core::fmt::Display;
use std::{panic, vec};
impl Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.id)
    }
}

lazy_static! {
    pub static ref EPSILON: Symbol = Symbol::new("epsilon");
    pub static ref EOT: Symbol = Symbol::new("EOT");
    pub static ref DOT: Symbol = Symbol::new(".");
    pub static ref UNKNOWN: Symbol = Symbol::new("UNKNOWN");
}

impl Symbol {
    pub fn new(id: &str) -> Symbol {
        Symbol { id: id.to_string() }
    }
}

pub struct Grammar {
    pub start: Symbol,
    pub productions: Vec<Production>,
    pub symbols: HashSet<Symbol>,
    terminals: HashSet<Symbol>,
}

impl Grammar {
    pub fn new(description: String) -> Grammar {
        let mut productions = Vec::new();
        let mut symbols = HashSet::new();

        for line in description.split("\n") {
            if line.trim().is_empty() {
                continue;
            }
            productions.push(Production::from(line))
        }

        for p in &productions {
            symbols.insert(p.lhs.clone());
            symbols.extend(p.rhs.clone());
        }
        symbols.remove(&Symbol::new("EOT"));

        let mut terminals = symbols.clone();
        for sym in &symbols {
            for p in &productions {
                if &p.lhs == sym {
                    terminals.remove(sym);
                }
            }
        }

        Grammar {
            start: productions[0].lhs.clone(),
            productions,
            symbols,
            terminals,
        }
    }

    // page 221 in the Dragon Book
    pub fn first(&self, sequence: &Vec<Symbol>) -> HashSet<Symbol> {
        let mut first_set = HashSet::new();
        let mut all_can_reduce_to_epsilon = true;
        for s in sequence {
            if self.is_terminal(s) {
                first_set.insert(s.clone());
                return first_set;
            } else {
                let prod = self.get_productions_of(s);
                for p in &prod {
                    first_set.extend(self.first(&p.rhs));
                }
            }
            if !self.can_reduce_to_epsilon(s) {
                all_can_reduce_to_epsilon = false;
                break;
            }
        }
        if all_can_reduce_to_epsilon {
            first_set.insert(EPSILON.clone());
        }
        return first_set;
    }

    // TODO cache results
    fn can_reduce_to_epsilon(&self, s: &Symbol) -> bool {
        let prod = self.get_productions_of(s);
        if self.is_terminal(s) {
            return false;
        }
        for p in prod {
            if p.rhs.len() == 1 && p.rhs.is_empty() {
                return true;
            }
            for rhs_sym in &p.rhs {
                if !self.can_reduce_to_epsilon(rhs_sym) {
                    return false;
                }
            }
        }
        true
    }

    pub fn get_productions_of(&self, s: &Symbol) -> Vec<Production> {
        let mut res = Vec::new();
        for p in &self.productions {
            if &p.lhs == s {
                res.push(p.clone());
            }
        }
        res
    }

    // page 245 in the Dragon Book
    pub fn is_kernel_item(&self, production: &Production) -> bool {
        let dot_idx = production.get_dot_index();
        if dot_idx == 0 && production.lhs == self.start {
            assert_eq!(production.rhs.len(), 2);
            return true;
        }

        if dot_idx > 0 && production.rhs[dot_idx - 1] != EOT.clone() {
            return true;
        }
        false
    }

    pub fn get_start_item(&self) -> Production {
        self.productions[0].clone()
    }

    pub fn is_terminal(&self, s: &Symbol) -> bool {
        self.terminals.contains(s) || s == &EOT.clone() || s == &UNKNOWN.clone()
    }

    // page 261
    pub fn goto_with_lookahead(
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

    pub fn move_dot_through(&self, item: &Production, symbol: &Symbol) -> Option<Production> {
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
    pub fn closure_with_lookahead(
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
                if self.is_terminal(sym_after_dot) {
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

                for prod in self.get_productions_of(sym_after_dot) {
                    for terminal in self.first(&postfix) {
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
    #[test]
    fn grammar_test() {
        let descript = r#"
            T -> program
            program ->
            program -> program declaration
            declaration -> varDecl
            declaration -> constDecl
            declaration -> statement
            "#
        .to_string();
        let g = Grammar::new(descript);

        let t = Symbol::new("T");
        let program = Symbol::new("program");
        let declaration = Symbol::new("declaration");
        let var_decl = Symbol::new("varDecl");
        let const_decl = Symbol::new("constDecl");
        let statement = Symbol::new("statement");

        assert_eq!(g.start, t);
        assert_eq!(g.productions[0].lhs, t);
        assert_eq!(g.productions[0].rhs, vec![program.clone()]);

        assert_eq!(g.productions[1].lhs, program);
        assert_eq!(g.productions[1].rhs, vec![]);

        assert_eq!(g.productions[2].lhs, program);
        assert_eq!(
            g.productions[2].rhs,
            vec![program.clone(), declaration.clone()]
        );

        assert_eq!(g.productions[3].lhs, declaration);
        assert_eq!(g.productions[3].rhs, vec![var_decl.clone()]);

        assert_eq!(g.productions[4].lhs, declaration);
        assert_eq!(g.productions[4].rhs, vec![const_decl.clone()]);

        assert_eq!(g.productions[5].lhs, declaration);
        assert_eq!(g.productions[5].rhs, vec![statement.clone()]);
    }

    #[test]
    fn first_test() {
        let descript = r#"
            S -> A Sp
            Sp -> Plus A Sp
            Sp ->
            A -> B Ap
            Ap -> Star B Ap
            Ap ->
            B -> LP S RP
            B -> c
            "#
        .to_string();
        let g = Grammar::new(descript);

        let s = Symbol::new("S");
        let a = Symbol::new("A");
        let sp = Symbol::new("Sp");
        let ap = Symbol::new("Ap");
        let b = Symbol::new("B");
        let lp = Symbol::new("LP");
        let rp = Symbol::new("RP");
        let c = Symbol::new("c");
        let star = Symbol::new("Star");
        let plus = Symbol::new("Plus");

        let f = g.first(&vec![s.clone()]);
        assert_eq!(f, HashSet::from([lp.clone(), c.clone()]));

        let f = g.first(&vec![a.clone()]);
        assert_eq!(f, HashSet::from([lp.clone(), c.clone()]));

        let f = g.first(&vec![b.clone()]);
        assert_eq!(f, HashSet::from([lp.clone(), c.clone()]));

        let f = g.first(&vec![ap.clone()]);
        assert_eq!(f, HashSet::from([star.clone(), EPSILON.clone()]));

        let f = g.first(&vec![sp.clone()]);
        assert_eq!(f, HashSet::from([plus.clone(), EPSILON.clone()]));

        let f = g.first(&vec![a.clone()]);
        assert_eq!(f, HashSet::from([lp.clone(), c.clone()]));

        let f = g.first(&vec![star.clone()]);
        assert_eq!(f, HashSet::from([star.clone()]));

        let f = g.first(&vec![lp.clone()]);
        assert_eq!(f, HashSet::from([lp.clone()]));

        let f = g.first(&vec![rp.clone()]);
        assert_eq!(f, HashSet::from([rp.clone()]));

        let f = g.first(&vec![c.clone()]);
        assert_eq!(f, HashSet::from([c.clone()]));

        let f = g.first(&vec![plus.clone()]);
        assert_eq!(f, HashSet::from([plus.clone()]));
    }

    #[test]
    fn closure_with_lookahead_test() {
        let descript = test_utils::get_test_grammar_3_description();

        let grammar = Grammar::new(descript);
        let mut starting_item = grammar.productions[0].clone();
        starting_item.rhs.insert(0, DOT.clone());
        let starting_item = ProductionWithLookahead::new(starting_item, EOT.clone());
        let item_set = HashSet::from([starting_item]);
        let closure = grammar.closure_with_lookahead(&item_set);

        for p in &closure {
            println!("{}", p);
        }

        let expected = HashSet::from([
            ProductionWithLookahead::from("S' -> . S", "EOT"),
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
        let starting_item_closure = grammar.closure_with_lookahead(&HashSet::from([starting_item]));
        let value = grammar.goto_with_lookahead(&starting_item_closure, &Symbol::new("C"));

        // page 262
        let expected = HashSet::from([
            ProductionWithLookahead::from("S -> C . C", "EOT"),
            ProductionWithLookahead::from("C -> . c C", "EOT"),
            ProductionWithLookahead::from("C -> . d", "EOT"),
        ]);
        assert_eq!(value, expected)
    }
}
