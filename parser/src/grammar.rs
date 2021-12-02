use std::collections::{HashSet, HashMap};
use lazy_static::lazy_static;
pub struct Grammar {
    pub start: Symbol,
    pub productions: Vec<Production>,
    pub symbols: HashSet<Symbol>,
    terminals: HashSet<Symbol>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Production {
    pub lhs: Symbol,
    pub rhs: Vec<Symbol>,
}

impl Production {
    pub fn find_indexes_of_symbol_on_rhs(&self, sym: &Symbol) -> Vec<usize> {
        let mut res = Vec::new();
        for (i, s) in self.rhs.iter().enumerate() {
            if s == sym {
                res.push(i);
            }
        }
        res
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

impl Display for ProductionWithLookahead {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}, {}", self.production, self.lookahead)?;
        Ok(())
    }
}



#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct Symbol {
    id: String,
}

use core::fmt::{Display};
use core::fmt;
impl Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.id)
    }
}

lazy_static! {
    pub static ref EPSILON: Symbol = Symbol::new("epsilon");
    pub static ref EOT: Symbol = Symbol::new("EOT");
}

impl Symbol {
    pub fn new(id: &str) -> Symbol {
        Symbol{id: id.to_string()}
    }
}

impl Grammar {
    
    pub fn new(description: String) -> Grammar {
        let description = description.chars();
        let mut productions = Vec::new();
        let mut token_string = String::new();
        let mut lhs = Symbol{ id: String::new() };
        let mut rhs = Vec::new();
        let mut symbols = HashSet::new();
        for c in description {     
            if c.is_alphabetic(){
                token_string.push(c);
            }
            if c.is_whitespace() {
                if lhs.id.is_empty() && !token_string.is_empty() {
                    lhs.id = token_string;
                    symbols.insert(lhs.clone());
                    token_string = String::new();
                } else if !token_string.is_empty() {
                    let sym = Symbol{ id: token_string };
                    rhs.push(sym.clone());
                    symbols.insert(sym.clone());
                    token_string = String::new();
                }
            }
            if c == '\n' && !(lhs.id.is_empty() && rhs.is_empty()) {
                let production = Production { lhs, rhs };
                productions.push(production);
                lhs = Symbol{ id: String::new() };
                rhs = Vec::new();
            }
        }

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
                break;
            }
        }
        return first_set;
    }

    // TODO(Krzysztof) cache results 
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


    pub fn is_terminal(&self, s: &Symbol) -> bool {
        self.terminals.contains(s)
    }


}


#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn grammar_test() {
        let descript = 
            r#"
            T -> program
            program -> epsilon
            program -> program declaration
            declaration -> varDecl
            declaration -> constDecl
            declaration -> statement
            "#.to_string();
        let g = Grammar::new(descript);

        let t =  Symbol::new("T");
        let program = Symbol::new("program");
        let declaration = Symbol::new("declaration");
        let var_decl = Symbol::new("varDecl");
        let const_decl = Symbol::new("constDecl");
        let statement = Symbol::new("statement");
        let epsilon = Symbol::new("epsilon");

        assert_eq!(g.start, t);
        assert_eq!(g.productions[0].lhs, t);
        assert_eq!(g.productions[0].rhs, vec![program.clone()]);

        assert_eq!(g.productions[1].lhs, program);
        assert_eq!(g.productions[1].rhs, vec![epsilon.clone()]);

        assert_eq!(g.productions[2].lhs, program);
        assert_eq!(g.productions[2].rhs, vec![program.clone(), declaration.clone()]);

        assert_eq!(g.productions[3].lhs, declaration);
        assert_eq!(g.productions[3].rhs, vec![var_decl.clone()]);

        assert_eq!(g.productions[4].lhs, declaration);
        assert_eq!(g.productions[4].rhs, vec![const_decl.clone()]);

        assert_eq!(g.productions[5].lhs, declaration);
        assert_eq!(g.productions[5].rhs, vec![statement.clone()]);
 
    }

    #[test]
    fn first_test() {
        let descript = 
            r#"
            S -> A Sp
            Sp -> Plus A Sp
            Sp -> epsilon
            A -> B Ap
            Ap -> Star B Ap
            Ap -> epsilon
            B -> LP S RP
            B -> c
            "#.to_string();
        let g = Grammar::new(descript);

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

        let f = g.first(&vec![s.clone()]);
        assert_eq!(f, HashSet::from([lp.clone(), c.clone()]));

        let f = g.first(&vec![a.clone()]);
        assert_eq!(f, HashSet::from([lp.clone(), c.clone()]));

        let f = g.first(&vec![b.clone()]);
        assert_eq!(f, HashSet::from([lp.clone(), c.clone()]));

        let f = g.first(&vec![ap.clone()]);
        assert_eq!(f, HashSet::from([star.clone(), epsilon.clone()]));

        let f = g.first(&vec![sp.clone()]);
        assert_eq!(f, HashSet::from([plus.clone(), epsilon.clone()]));

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
}