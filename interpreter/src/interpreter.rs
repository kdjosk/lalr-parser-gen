use crate::visitor::*;
use crate::ast::*;
use std::collections::{HashMap, HashSet};
use std::panic;

#[derive(Clone, Debug)]
enum Value {
    U8(u8),
    U32(u32),
    U64(u64),
    I32(i32),
    I64(i64),
    F32(f32),
    F64(f64),
    Str(String),
    Bool(bool),
}

struct Scope {
    variables: HashMap<Name, Value>,
    fun_defs: HashMap<Name, FunDef>,
}

impl Scope {
    pub fn new() -> Scope {
        Scope { 
            variables: HashMap::new(),
            fun_defs: HashMap::new(),
        }
    }

    pub fn get_value(&self, name: &Name) -> Option<&Value> {
        self.variables.get(name)
    }

    pub fn add_variable(&mut self, name: Name, value: Value) {
        self.variables.insert(name, value);
    }

    pub fn get_fun_def(&self, name: &Name) -> Option<&FunDef> {
        self.fun_defs.get(name)
    }

    pub fn add_fun_def(&mut self, fun_def: FunDef) {
        self.fun_defs.insert(fun_def.name.clone(), fun_def);
    }
}

struct Context {
    scope_hierarchy: Vec<Scope>,
}

impl Context {
    pub fn new() -> Context {
        Context { 
            scope_hierarchy: Vec::new()
        }
    }

    pub fn try_find_variable_in_scope_hierarchy(&self, name: &Name) -> Option<&Value> {
        for scope in self.scope_hierarchy.iter().rev() {
            if let Some(v) = scope.get_value(name) {
                return Some(v);
            }
        }
        None
    }

    pub fn try_find_fun_def_in_scope_hierarchy(&self, name: &Name) -> Option<&FunDef> {
        for scope in self.scope_hierarchy.iter().rev() {
            if let Some(f) = scope.get_fun_def(name) {
                return Some(f);
            }
        }
        None
    }

    pub fn add_variable_to_current_scope(&mut self, name: Name, value: Value) {
        self.scope_hierarchy.last_mut().unwrap().add_variable(name, value);
    }

    pub fn add_fun_def_to_current_scope(&mut self, fun_def: FunDef) {
        self.scope_hierarchy.last_mut().unwrap().add_fun_def(fun_def);
    }
}

pub struct Interpreter {
    call_context_stack: Vec<Context>,
    last_expr_value: Option<Value>,
    next_bin_op: Option<BinOp>,
    next_un_op: Option<UnOp>,
    variable_type: Option<Type>,
    args: Vec<(Option<Name>, Value)>
}

impl Interpreter {
    pub fn new() -> Interpreter{
        Interpreter {
            call_context_stack: Vec::new(),
            last_expr_value: None,
            next_bin_op: None,
            next_un_op: None,
            variable_type: None,
            args: Vec::new(),
        }
    }

    fn retrieve_variable_value(&self, name: &Name) -> Value {
        let ctx = self.call_context_stack.last().unwrap();
        if let Some(v) = ctx.try_find_variable_in_scope_hierarchy(name) {
            return v.clone();
        }
        panic!("Use of undeclared variable {:?}", name);
    }

    fn push_new_context_and_instantiate_params(&mut self, name: &Name) {
        let ctx = self.call_context_stack.last().unwrap();
        let mut new_ctx = Context::new();
        if let Some(f) = ctx.try_find_fun_def_in_scope_hierarchy(name) {
            let param_name_to_val = self.map_params_to_value(&f.params);
            for (param_name, val) in param_name_to_val {
                new_ctx.add_variable_to_current_scope(param_name, val);
            }
        }
        self.call_context_stack.push(new_ctx);
    }

    fn map_params_to_value(&self, params: &Vec<Param>) -> HashMap<Name, Value> {
        let mut first_keyword_arg_appeared = false;
        let mut params_left: HashSet<_> = params.clone().into_iter().collect();
        if params_left.len() < params.len() {
            panic!("All function parameters should be unique");
        }

        let mut param_name_to_val = HashMap::new();
        let mut param_name;

        for (arg_idx, (kword, arg_val)) in self.args.iter().enumerate() {   
            if let Some(kw) = kword {
                if !first_keyword_arg_appeared {
                    first_keyword_arg_appeared = true;
                } 
                param_name = kw;
            } else {
                if first_keyword_arg_appeared {
                    panic!("Unexpected unnamed argument after a named argument");
                } else {
                   param_name = &params[arg_idx].name;
                }
            }

            let param_to_remove;
            match arg_val {
                Value::U8(_) => {
                    param_to_remove = Param::new(param_name.clone(), Type::Uint8); 
                }
                Value::U32(_) => {
                    param_to_remove = Param::new(param_name.clone(), Type::Uint32);
                }
                Value::U64(_) => {
                    param_to_remove = Param::new(param_name.clone(), Type::Uint64);
                }
                Value::I32(_) => {
                    param_to_remove = Param::new(param_name.clone(), Type::Int32);
                }
                Value::I64(_) => {
                    param_to_remove = Param::new(param_name.clone(), Type::Int64);
                }
                Value::F32(_) => {
                    param_to_remove = Param::new(param_name.clone(), Type::Float32);
                }
                Value::F64(_) => {
                    param_to_remove = Param::new(param_name.clone(), Type::Float64);
                }
                Value::Str(_) => {
                    param_to_remove = Param::new(param_name.clone(), Type::String);
                }
                Value::Bool(_) => {
                    param_to_remove = Param::new(param_name.clone(), Type::Bool);
                }
            }
            if !params_left.remove(&param_to_remove) {
                panic!("Can't find param of type {:?} and name {:?}", param_to_remove.typ, kword)
            } else {
                param_name_to_val.insert(param_to_remove.name, arg_val.clone());
            }
        }
        param_name_to_val
    }

    fn perform_binary_op(&self, op: BinOp, lhs: &Value, rhs: &Value) -> Value {
        match op {
            BinOp::Add => self.perform_add(lhs, rhs),
            BinOp::Sub => self.perform_sub(lhs, rhs),
            BinOp::Mult => self.perform_mult(lhs, rhs),
            BinOp::Div => self.perform_div(lhs, rhs),
            BinOp::GreaterEq => self.perform_greater_eq(lhs, rhs),
            BinOp::LessEq => self.perform_less_eq(lhs, rhs),
            BinOp::Greater => self.perform_greater(lhs, rhs),
            BinOp::Less => self.perform_less(lhs, rhs),
            BinOp::Eq => self.perform_eq(lhs, rhs),
            BinOp::LogicOr => self.perform_logic_or(lhs, rhs),
            BinOp::LogicAnd => self.perform_logic_and(lhs, rhs),
        }
    }

    fn perform_unary_op(&self, op: UnOp, v: &Value) -> Value {
        match op {
            UnOp::Minus => self.perform_minus(v),
            UnOp::LogicNot => self.perform_logic_not(v),
        }
    }

    fn perform_add(&self, lhs: &Value, rhs: &Value) -> Value 
    {
        match (lhs, rhs) {
            (Value::F32(l), Value::F32(r)) => Value::F32(l + r),
            (Value::F64(l), Value::F64(r)) => Value::F64(l + r),
            (Value::U32(l), Value::U32(r)) => Value::U32(l + r),
            (Value::U64(l), Value::U64(r)) => Value::U64(l + r),
            (Value::I32(l), Value::I32(r)) => Value::I32(l + r),
            (Value::I64(l), Value::I64(r)) => Value::I64(l + r),
            (Value::Str(l), Value::Str(r)) => Value::Str(self.add_strings(l.clone(), r)),
            _ => panic!("Unsupported types for addition: {:?}, {:?}", lhs, rhs),
        }
    }

    fn add_strings(&self, mut lhs: String, rhs: &String) -> String {
        lhs.push_str(rhs.as_str());
        lhs
    }

    fn perform_sub(&self, lhs: &Value, rhs: &Value) -> Value 
    {
        match (lhs, rhs) {
            (Value::F32(l), Value::F32(r)) => Value::F32(l - r),
            (Value::F64(l), Value::F64(r)) => Value::F64(l - r),
            (Value::U32(l), Value::U32(r)) => Value::U32(l - r),
            (Value::U64(l), Value::U64(r)) => Value::U64(l - r),
            (Value::I32(l), Value::I32(r)) => Value::I32(l - r),
            (Value::I64(l), Value::I64(r)) => Value::I64(l - r),
            _ => panic!("Unsupported types for substraction: {:?}, {:?}", lhs, rhs),
        }
    }

    fn perform_mult(&self, lhs: &Value, rhs: &Value) -> Value 
    {
        match (lhs, rhs) {
            (Value::F32(l), Value::F32(r)) => Value::F32(l * r),
            (Value::F64(l), Value::F64(r)) => Value::F64(l * r),
            (Value::U32(l), Value::U32(r)) => Value::U32(l * r),
            (Value::U64(l), Value::U64(r)) => Value::U64(l * r),
            (Value::I32(l), Value::I32(r)) => Value::I32(l * r),
            (Value::I64(l), Value::I64(r)) => Value::I64(l * r),
            _ => panic!("Unsupported types for multiplication: {:?}, {:?}", lhs, rhs),
        }
    }

    fn perform_div(&self, lhs: &Value, rhs: &Value) -> Value 
    {
        match (lhs, rhs) {
            (Value::F32(l), Value::F32(r)) => Value::F32(l / r),
            (Value::F64(l), Value::F64(r)) => Value::F64(l / r),
            (Value::U32(l), Value::U32(r)) => Value::U32(l / r),
            (Value::U64(l), Value::U64(r)) => Value::U64(l / r),
            (Value::I32(l), Value::I32(r)) => Value::I32(l / r),
            (Value::I64(l), Value::I64(r)) => Value::I64(l / r),
            _ => panic!("Unsupported types for division: {:?}, {:?}", lhs, rhs),
        }
    }

    fn perform_greater_eq(&self, lhs: &Value, rhs: &Value) -> Value 
    {
        match (lhs, rhs) {
            (Value::F32(l), Value::F32(r)) => Value::Bool(l >= r),
            (Value::F64(l), Value::F64(r)) => Value::Bool(l >= r),
            (Value::U32(l), Value::U32(r)) => Value::Bool(l >= r),
            (Value::U64(l), Value::U64(r)) => Value::Bool(l >= r),
            (Value::I32(l), Value::I32(r)) => Value::Bool(l >= r),
            (Value::I64(l), Value::I64(r)) => Value::Bool(l >= r),
            _ => panic!("Unsupported types for comparison: {:?}, {:?}", lhs, rhs),
        }
    }

    fn perform_less_eq(&self, lhs: &Value, rhs: &Value) -> Value 
    {
        match (lhs, rhs) {
            (Value::F32(l), Value::F32(r)) => Value::Bool(l <= r),
            (Value::F64(l), Value::F64(r)) => Value::Bool(l <= r),
            (Value::U32(l), Value::U32(r)) => Value::Bool(l <= r),
            (Value::U64(l), Value::U64(r)) => Value::Bool(l <= r),
            (Value::I32(l), Value::I32(r)) => Value::Bool(l <= r),
            (Value::I64(l), Value::I64(r)) => Value::Bool(l <= r),
            _ => panic!("Unsupported types for comparison: {:?}, {:?}", lhs, rhs),
        }
    }

    fn perform_greater(&self, lhs: &Value, rhs: &Value) -> Value 
    {
        match (lhs, rhs) {
            (Value::F32(l), Value::F32(r)) => Value::Bool(l > r),
            (Value::F64(l), Value::F64(r)) => Value::Bool(l > r),
            (Value::U32(l), Value::U32(r)) => Value::Bool(l > r),
            (Value::U64(l), Value::U64(r)) => Value::Bool(l > r),
            (Value::I32(l), Value::I32(r)) => Value::Bool(l > r),
            (Value::I64(l), Value::I64(r)) => Value::Bool(l > r),
            _ => panic!("Unsupported types for comparison: {:?}, {:?}", lhs, rhs),
        }
    }

    fn perform_less(&self, lhs: &Value, rhs: &Value) -> Value 
    {
        match (lhs, rhs) {
            (Value::F32(l), Value::F32(r)) => Value::Bool(l < r),
            (Value::F64(l), Value::F64(r)) => Value::Bool(l < r),
            (Value::U32(l), Value::U32(r)) => Value::Bool(l < r),
            (Value::U64(l), Value::U64(r)) => Value::Bool(l < r),
            (Value::I32(l), Value::I32(r)) => Value::Bool(l < r),
            (Value::I64(l), Value::I64(r)) => Value::Bool(l < r),
            _ => panic!("Unsupported types for comparison: {:?}, {:?}", lhs, rhs),
        }
    }

    fn perform_eq(&self, lhs: &Value, rhs: &Value) -> Value 
    {
        match (lhs, rhs) {
            (Value::F32(l), Value::F32(r)) => Value::Bool(l == r),
            (Value::F64(l), Value::F64(r)) => Value::Bool(l == r),
            (Value::U32(l), Value::U32(r)) => Value::Bool(l == r),
            (Value::U64(l), Value::U64(r)) => Value::Bool(l == r),
            (Value::I32(l), Value::I32(r)) => Value::Bool(l == r),
            (Value::I64(l), Value::I64(r)) => Value::Bool(l == r),
            _ => panic!("Unsupported types for comparison: {:?}, {:?}", lhs, rhs),
        }
    }

    fn perform_logic_and(&self, lhs: &Value, rhs: &Value) -> Value 
    {
        match (lhs, rhs) {
            (Value::Bool(l), Value::Bool(r)) => Value::Bool(*l && *r),
            _ => panic!("Unsupported types for boolean operation: {:?}, {:?}", lhs, rhs),
        }
    }

    fn perform_logic_or(&self, lhs: &Value, rhs: &Value) -> Value 
    {
        match (lhs, rhs) {
            (Value::Bool(l), Value::Bool(r)) => Value::Bool(*l || *r),
            _ => panic!("Unsupported types for boolean operation: {:?}, {:?}", lhs, rhs),
        }
    }

    fn perform_logic_not(&self, val: &Value) -> Value 
    {
        match val {
            Value::Bool(val) => Value::Bool(!val),
            _ => panic!("Unsupported type for boolean operation: {:?}", val),
        }
    }

    fn perform_minus(&self, val: &Value) -> Value 
    {
        match val {
            Value::F32(v) => Value::F32(-v),
            Value::F64(v) => Value::F64(-v),
            Value::I32(v) => Value::I32(-v),
            Value::I64(v) => Value::I64(-v),
            _ => panic!("Type {:?} can't be negative", val),
        }
    }
}

impl AstVisitor for Interpreter {
    fn visit_program(&mut self, p: &Program) {
        walk_program(self, p)
    }

    fn visit_stmt(&mut self, s: &Stmt) {
        match s {
            Stmt::If(if_block) => {
                self.visit_expr(&if_block.expr);

            },
            Stmt::Expr(_) => todo!(),
            Stmt::Assignment(_, _) => todo!(),
            Stmt::FunDef(_) => todo!(),
            Stmt::VarDecl(_, _, _) => todo!(),
            Stmt::ForLoop(_) => todo!(),
            Stmt::Return(_) => todo!(),
        }
    }

    fn visit_expr(&mut self, e: &Expr) {
        match e {
            Expr::IntLit(i) => self.visit_int_lit(*i),
            Expr::StrLit(s) => self.visit_str_lit(s),
            Expr::FloatLit(f) => self.visit_float_lit(*f),
            Expr::BoolLit(b) => self.visit_bool_lit(*b),
            Expr::Identifier(id) => self.visit_name(id),
            Expr::Binary(op, lhs, rhs) => {
                self.visit_expr(lhs);
                let lhs_val = self.last_expr_value.clone();
                self.visit_expr(rhs);
                let rhs_val = self.last_expr_value.clone();
                self.visit_bin_op(*op);
                let op_val = self.next_bin_op;
                match (op_val, &lhs_val, &rhs_val) {
                    (Some(op), Some(l), Some(r)) => {
                        self.last_expr_value = Some(self.perform_binary_op(op, l, r));
                    }
                    _ => panic!("Missing component for binary operation op: {:?}, lhs: {:?}, rhs: {:?}",
                                 op_val, lhs_val, rhs_val)
                }
            }
            Expr::Unary(op, expr) => {
                self.visit_expr(expr);
                let val = self.last_expr_value.clone();
                self.visit_un_op(*op);
                let op_val = self.next_un_op;
                match (op_val, &val) {
                    (Some(op_val), Some(val)) => {
                        self.last_expr_value = Some(self.perform_unary_op(op_val, val))
                    }
                    _ => panic!("Missing component for unary operation op: {:?}, val: {:?}",
                                 op_val, val)
                }
            },
            Expr::Call(expr) => {
                self.visit_call_expr(expr);
            },
        }
    }

    fn visit_if_block(&mut self, i: &IfBlock) {
        walk_if_block(self, i)
    }

    fn visit_call_expr(&mut self, c: &CallExpr) {
        // let fn_name = c.name;
    }

    fn visit_else_tail(&mut self, e: &ElseTail) {
        walk_else_tail(self, e)
    }

    fn visit_else_block(&mut self, e: &ElseBlock) {
        walk_else_block(self, e)
    }

    fn visit_fun_def(&mut self, f: &FunDef) {
        walk_fun_def(self, f)
    }

    fn visit_for_loop(&mut self, f: &ForLoopBlock) {
        walk_for_loop_block(self, f)
    }

    fn visit_param(&mut self, p: &Param) {
        walk_param(self, p)
    }

    fn visit_arg(&mut self, arg: &Arg) {
        walk_arg(self, arg)
    }

    fn visit_int_lit(&mut self, i: u64) {
        self.last_expr_value = Some(Value::U64(i));
    }

    fn visit_str_lit(&mut self, s: &String) {
        self.last_expr_value = Some(Value::Str(s.clone()));
    }

    fn visit_float_lit(&mut self, f: f64) {
        self.last_expr_value = Some(Value::F64(f));
    }

    fn visit_bool_lit(&mut self, b: bool) {
        self.last_expr_value = Some(Value::Bool(b));
    }

    fn visit_name(&mut self, n: &Name) {}

    fn visit_bin_op(&mut self, o: BinOp) {
        self.next_bin_op = Some(o);
    }

    fn visit_un_op(&mut self, u: UnOp) {
        self.next_un_op = Some(u);
    }

    fn visit_type(&mut self, t: &Type) {
        self.variable_type = Some(t.clone());
    }
}

