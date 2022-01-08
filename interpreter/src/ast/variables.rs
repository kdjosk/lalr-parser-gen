struct Variable {
    name: String,
    value: Box<dyn VariableValue>,
    var_type: Box<dyn VariableType>,
}

pub trait VariableValue {}
pub trait VariableType {}
