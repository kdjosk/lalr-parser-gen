trait Expression {} 

impl Expression for FunctionCall {}
struct FunctionCall {

}

struct FunctionCallContext {
  scope: Vec<Scope>,
}

struct Scope {
  // variables: VariableMap,
  // function_definitions: FunctionDefinitionMap,
}

