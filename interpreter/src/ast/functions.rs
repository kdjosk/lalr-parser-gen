use std::collections::HashMap;

use super::variables::{VariableType, VariableValue};
use super::{Expression, Statement};

impl Expression for FunctionCall {}
pub struct FunctionCall {
    arguments: HashMap<String, Box<dyn VariableValue>>,
}

impl Statement for FunctionDefinition {}
pub struct FunctionDefinition {
    parameters: HashMap<String, Box<dyn VariableType>>,
}

pub struct FunctionCallContext {
    scope: Vec<Scope>,
}

pub struct Scope {
    variables: HashMap<String, Box<dyn VariableValue>>,
    functions: HashMap<String, FunctionDefinition>,
}

impl Scope {
    pub fn new() -> Scope {
        Scope {
            variables: HashMap::new(),
            functions: HashMap::new(),
        }
    }

    pub fn add_variable_definition(&mut self, name: String, value_ptr: Box<dyn VariableValue>) {
        self.variables.insert(name, value_ptr);
    }

    pub fn add_function_definition(&mut self, name: String, def: FunctionDefinition) {
        self.functions.insert(name, def);
    }
}
