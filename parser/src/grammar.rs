use std::collections::{HashSet, HashMap};
pub struct Grammar {
    pub start: Symbol,
    pub productions: Vec<Production>,
    symbols: HashSet<Symbol>,
    terminals: HashSet<Symbol>,
}

#[derive(Debug, Clone)]
pub struct Production {
    pub lhs: Symbol,
    pub rhs: Vec<Symbol>,
}

impl Production {
    pub fn find_indexes_of_symbol_on_rhs(&self, s: &Symbol) -> Vec<usize> {
        let mut res = Vec::new();
        for i in [self.rhs.len()] {
            if s == &self.rhs[i] {
                res.push(i);
            }
        }
        res
    }
}

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct Symbol {
    id: String,
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

    pub fn get_first_sets(&self) -> HashMap<Symbol, HashSet<Symbol>> {
        let mut f = HashMap::new();
        for s in &self.symbols {
            f.insert(s.clone(), self.first(&s));
        }
        f
    }

    fn first(&self, s: &Symbol) -> HashSet<Symbol> {
        let mut first_set = HashSet::new();
        if self.is_terminal(s) {
            first_set.insert(s.clone());
            return first_set;
        } else {
            let prod = self.get_productions_of(s);
            for p in &prod {
                for sym in &p.rhs {
                    if sym == s {
                        if self.is_nullable(s) {
                            continue;
                        } else {
                            break;
                        }
                    }
                    first_set.extend(self.first(sym));
                    if !self.is_nullable(sym) {
                        break;
                    }  
                }
            }
        }
        if self.is_nullable(s) {
            first_set.insert(Symbol{id: "epsilon".to_string()});
        } 

        return first_set;
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

    pub fn is_nullable(&self, s: &Symbol) -> bool {
        for p in &self.productions {
            if &p.lhs == s && p.rhs.is_empty() {
                return true;
            }
        }
        false
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
            program ->
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

        assert_eq!(g.start, t);
        assert_eq!(g.productions[0].lhs, t);
        assert_eq!(g.productions[0].rhs, vec![program.clone()]);

        assert_eq!(g.productions[1].lhs, program);
        assert_eq!(g.productions[1].rhs, vec![]);

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
            Sp ->
            A -> B Ap
            Ap -> Star B Ap
            Ap ->
            B -> LP S RP
            B -> a
            "#.to_string();
        let g = Grammar::new(descript);
        let f = g.get_first_sets();

        let S =  Symbol::new("S");
        let A = Symbol::new("A");
        let Sp = Symbol::new("Sp");
        let Ap = Symbol::new("Ap");
        let B = Symbol::new("B");
        let LP = Symbol::new("LP");
        let RP = Symbol::new("RP");
        let a = Symbol::new("a");
        let Star = Symbol::new("Star");
        let Plus = Symbol::new("Plus");
        let epsilon = Symbol::new("epsilon");
        
        assert_eq!(f.get(&S).unwrap(), &HashSet::from([LP.clone(), a.clone()]));
        assert_eq!(f.get(&A).unwrap(), &HashSet::from([LP.clone(), a.clone()]));
        assert_eq!(f.get(&B).unwrap(), &HashSet::from([LP.clone(), a.clone()]));
        assert_eq!(f.get(&Ap).unwrap(), &HashSet::from([Star.clone(), epsilon.clone()]));
        assert_eq!(f.get(&Sp).unwrap(), &HashSet::from([Plus.clone(), epsilon.clone()]));
        assert_eq!(f.get(&Star).unwrap(), &HashSet::from([Star.clone()]));
        assert_eq!(f.get(&LP).unwrap(), &HashSet::from([LP.clone()]));
        assert_eq!(f.get(&RP).unwrap(), &HashSet::from([RP.clone()]));
        assert_eq!(f.get(&a).unwrap(), &HashSet::from([a.clone()]));
        assert_eq!(f.get(&Plus).unwrap(), &HashSet::from([Plus.clone()]));
        
    }
}