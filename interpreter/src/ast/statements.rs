use super::Statement;
use std::fmt::Binary;

impl Statement for ConditionalStatement {}
struct ConditionalStatement {
    condition: Box<dyn Expression>,
    true_block: Vec<Box<dyn Statement>>,
    false_block: Vec<Box<dyn Statement>>,
}

impl Statement for ExpressionStatement {}
struct ExpressionStatement {
    expression: Box<dyn Expression>,
}

impl Statement for AssignmentStatement {}
struct AssignmentStatement {}

trait Expression {}
