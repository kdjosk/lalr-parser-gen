use crate::ast::*;
pub trait AstVisitor: Sized {
    fn visit_program(&mut self, p: &Program) {
        walk_program(self, p)
    }

    fn visit_stmt(&mut self, s: &Stmt) {
        walk_stmt(self, s)
    }

    fn visit_expr(&mut self, e: &Expr) {
        walk_expr(self, e)
    }

    fn visit_if_block(&mut self, i: &IfBlock) {
        walk_if_block(self, i)
    }

    fn visit_int_lit(&mut self, i: u64);

    fn visit_str_lit(&mut self, s: &String);

    fn visit_float_lit(&mut self, f: f64);

    fn visit_bool_lit(&mut self, b: bool);

    fn visit_name(&mut self, n: &Name);

    fn visit_bin_op(&mut self, o: BinOp);

    fn visit_un_op(&mut self, u: UnOp);

    fn visit_call_expr(&mut self, c: &CallExpr) {
        walk_call_expr(self, c)
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

    fn visit_type(&mut self, t: &Type);

    fn visit_arg(&mut self, a: &Arg) {
        walk_arg(self, a)
    }

}

pub fn walk_program<V: AstVisitor>(visitor: &mut V, program: &Program) {
    for stmt in &program.stmt_seq {
        visitor.visit_stmt(stmt);
    }
}

pub fn walk_stmt<V: AstVisitor>(visitor: &mut V, stmt: &Stmt) {
    match stmt {
        Stmt::If(if_block) => visitor.visit_if_block(if_block),
        Stmt::Expr(expr) => visitor.visit_expr(expr),
        Stmt::Assignment(name, expr) => {
            visitor.visit_name(name);
            visitor.visit_expr(expr)
        }
        Stmt::FunDef(fun_def) => visitor.visit_fun_def(fun_def),
        Stmt::VarDecl(name, expr) => {
            visitor.visit_name(name);
            visitor.visit_expr(expr)
        }
        Stmt::ForLoop(for_loop) => visitor.visit_for_loop(for_loop),
        Stmt::Return(expr) => visitor.visit_expr(expr),
    }
}

pub fn walk_expr<V: AstVisitor>(visitor: &mut V, expr: &Expr) {
    match expr {
        Expr::IntLit(i) => visitor.visit_int_lit(*i),
        Expr::StrLit(s) => visitor.visit_str_lit(s),
        Expr::FloatLit(f) => visitor.visit_float_lit(*f),
        Expr::BoolLit(b) => visitor.visit_bool_lit(*b),
        Expr::Identifier(n) => visitor.visit_name(n),
        Expr::Binary(op, l, r) => {
            visitor.visit_bin_op(*op);
            visitor.visit_expr(l);
            visitor.visit_expr(r)
        }
        Expr::Unary(op, expr) => {
            visitor.visit_un_op(*op);
            visitor.visit_expr(expr)
        },
        Expr::Call(call) => {
            visitor.visit_call_expr(call)
        },
    }
}

pub fn walk_if_block<V: AstVisitor>(visitor: &mut V, if_block: &IfBlock) {
    visitor.visit_expr(&if_block.expr);
    for stmt in &if_block.stmt_seq {
        visitor.visit_stmt(stmt)
    }
    if let Some(ref el) = if_block.else_tail {
        visitor.visit_else_tail(el)
    }
    
}

pub fn walk_else_tail<V: AstVisitor>(visitor: &mut V, else_tail: &ElseTail) {
    if let Some(ref eif) = else_tail.else_if_block {
        visitor.visit_if_block(eif)
    }
    if let Some(ref el) = else_tail.else_block {
        visitor.visit_else_block(el)
    }
}

pub fn walk_else_block<V: AstVisitor>(visitor: &mut V, else_block: &ElseBlock) {
    for stmt in &else_block.stmt_seq {
        visitor.visit_stmt(stmt)
    }
}

pub fn walk_fun_def<V: AstVisitor>(visitor: &mut V, fun_def: &FunDef) {
    visitor.visit_name(&fun_def.name);
    for param in &fun_def.params {
        visitor.visit_param(param);
    }
    visitor.visit_type(&fun_def.ret_type);
    for stmt in &fun_def.stmt_seq {
        visitor.visit_stmt(stmt)
    }
}

pub fn walk_for_loop_block<V: AstVisitor>(visitor: &mut V, for_loop: &ForLoopBlock) {
    visitor.visit_name(&for_loop.iterator);
    visitor.visit_expr(&for_loop.iterable);
    for stmt in &for_loop.stmt_seq {
        visitor.visit_stmt(stmt)
    }
}

pub fn walk_call_expr<V: AstVisitor>(visitor: &mut V, call: &CallExpr) {
    visitor.visit_name(&call.name);
    for arg in &call.args {
        visitor.visit_arg(arg)
    }
}

pub fn walk_arg<V: AstVisitor>(visitor: &mut V, arg: &Arg) {
    if let Some(ref kw) = arg.kword {
        visitor.visit_name(kw);
    }
    visitor.visit_expr(&arg.expr);
}

pub fn walk_param<V: AstVisitor>(visitor: &mut V, param: &Param) {
    visitor.visit_name(&param.name);
    visitor.visit_type(&param.ptype)
}
