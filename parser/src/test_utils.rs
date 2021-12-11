use crate::grammar::Production;
use std::collections::HashSet;

pub fn get_test_grammar_1_description() -> String {
    r#"
    T -> S
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

pub fn get_test_grammar_2_description() -> String {
    r#"
    S' -> S
    S -> L Eq R
    S -> R
    L -> Star R
    L -> id
    R -> L
    "#
    .to_string()
}

// page 263
pub fn get_test_grammar_3_description() -> String {
    r#"
    S' -> S
    S -> C C
    C -> c C
    C -> d
    "#
    .to_string()
}

pub fn assert_vec_of_sets_of_items_eq(
    v1: &Vec<HashSet<Production>>,
    v2: &Vec<HashSet<Production>>,
) {
    assert_eq!(v1.len(), v2.len());
    for set in v1 {
        assert!(v2.contains(set), "No set {:?}", set);
    }
}
