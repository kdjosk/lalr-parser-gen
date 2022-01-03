use std::ops::IndexMut;

use lexer::Token;
// page 318

pub trait Node {
    fn get_label(&self) -> String;
    fn get_children(&self) -> Option<&Vec<Box<dyn Node>>>;
}

impl Node for Interior {
    fn get_label(&self) -> String {
        self.operation.get_name()
    }
    fn get_children(&self) -> Option<&Vec<Box<dyn Node>>> {
        Some(&self.children)
    }
}
pub struct Interior {
    operation: Box<dyn Operation>,
    children: Vec<Box<dyn Node>>,
}
impl Interior {
    pub fn new(operation: Box<dyn Operation>, children: Vec<Box<dyn Node>>) -> Interior {
        Interior {
            operation,
            children,
        }
    }

    pub fn add_child(&mut self, child: Box<dyn Node>) {
        self.children.push(child);
    }

    pub fn add_children(&mut self, children: Vec<Box<dyn Node>>) {
        self.children.extend(children);
    }
}


pub struct Leaf<T: ToString> {
    lexical_value: T,
}
impl<T: ToString> Node for Leaf<T> {
    fn get_label(&self) -> String {
        self.lexical_value.to_string()
    }
    fn get_children(&self) -> Option<&Vec<Box<dyn Node>>> {
        None
    }
}
impl<T: ToString> Leaf<T>{
    pub fn new(lexical_value: T) -> Leaf<T> {
        Leaf {
            lexical_value,
        }
    }
}

pub trait Operation {
    fn get_name(&self) -> String;
}

pub struct Program {

}
impl Operation for Program {
    fn get_name(&self) -> String {
        return "program".to_string()
    }
}

pub struct StatementSeqence {

}
impl Operation for StatementSeqence {
    fn get_name(&self) -> String {
        return "StatementSequence".to_string()
    }
}
pub struct Statement {

}
impl Operation for Statement{
    fn get_name(&self) -> String {
        return "Statement".to_string()
    }
}

pub struct ExpressionStatement {

}
impl Operation for ExpressionStatement {
    fn get_name(&self) -> String {
        return "ExpressionStatement".to_string()
    }
}
pub struct AssignmentStatement {

}
impl Operation for AssignmentStatement {
    fn get_name(&self) -> String {
        return "AssignmentStatement".to_string()
    }
}

pub struct ConditionalStatement {

}
impl Operation for ConditionalStatement {
    fn get_name(&self) -> String {
        return "ConditionalStatement".to_string()
    }
}

pub struct Expression {

}
impl Operation for Expression {
    fn get_name(&self) -> String {
        return "Expression".to_string()
    }
}

pub struct Disjunction {

}
impl Operation for Disjunction {
    fn get_name(&self) -> String {
        return "Disjunction".to_string()
    }
}

pub struct ConjunctionSequence {

}
impl Operation for ConjunctionSequence {
    fn get_name(&self) -> String {
        return "ConjunctionSequence".to_string()
    }
}

pub struct Conjunction {

}
impl Operation for Conjunction {
    fn get_name(&self) -> String {
        return "Conjunction".to_string()
    }
}
pub struct InversionSequence {

}
impl Operation for InversionSequence {
    fn get_name(&self) -> String {
        return "InversionSequence".to_string()
    }
}

pub struct Inversion {

}
impl Operation for Inversion {
    fn get_name(&self) -> String {
        return "Inversion".to_string()
    }
}

pub struct Comparison {

}
impl Operation for Comparison {
    fn get_name(&self) -> String {
        return "Comparison".to_string()
    }
}

pub struct Sum {

}
impl Operation for Sum {
    fn get_name(&self) -> String {
        return "Sum".to_string()
    }
}

pub struct Term {

}
impl Operation for Term {
    fn get_name(&self) -> String {
        return "Term".to_string()
    }
}

pub struct Factor {

}
impl Operation for Factor {
    fn get_name(&self) -> String {
        return "Factor".to_string()
    }
}

pub struct Atom {

}
impl Operation for Atom {
    fn get_name(&self) -> String {
        return "Atom".to_string()
    }
}

pub struct RelationalOperator {

}
impl Operation for RelationalOperator {
    fn get_name(&self) -> String {
        return "RelationalOperator".to_string()
    }
}

pub struct MultiplicativeOperator {

}
impl Operation for MultiplicativeOperator {
    fn get_name(&self) -> String {
        return "MultiplicativeOperator".to_string()
    }
}

pub struct AdditiveOperator {

}
impl Operation for AdditiveOperator {
    fn get_name(&self) -> String {
        return "AdditiveOperator".to_string()
    }
}
pub struct Literal {

}
impl Operation for Literal {
    fn get_name(&self) -> String {
        return "Literal".to_string()
    }
}

pub struct BooleanLiteral {

}
impl Operation for BooleanLiteral {
    fn get_name(&self) -> String {
        return "BooleanLiteral".to_string()
    }
}


