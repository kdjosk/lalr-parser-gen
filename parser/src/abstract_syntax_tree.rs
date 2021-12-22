use std::ops::IndexMut;

use lexer::Token;
// page 318

pub trait Node {
    fn get_label(&self) -> String;
    fn get_children(&self) -> Option<&Vec<Box<dyn Node>>>;
}

impl Node for Interior {
    fn get_label(&self) -> String {
        self.operation.clone()
    }
    fn get_children(&self) -> Option<&Vec<Box<dyn Node>>> {
        Some(&self.children)
    }
}
pub struct Interior {
    operation: String,
    children: Vec<Box<dyn Node>>,
}
impl Interior {
    pub fn new(operation: String, children: Vec<Box<dyn Node>>) -> Interior {
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


pub struct Leaf<T> {
    operation: String,
    lexical_value: T,
}
impl<T> Node for Leaf<T> {
    fn get_label(&self) -> String {
        self.operation.clone()
    }
    fn get_children(&self) -> Option<&Vec<Box<dyn Node>>> {
        None
    }
}
impl<T> Leaf<T>{
    pub fn new(operation: String, lexical_value: T) -> Leaf<T> {
        Leaf {
            operation,
            lexical_value,
        }
    }
}



