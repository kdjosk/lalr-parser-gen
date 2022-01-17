use std::rc::Rc;

use crate::visitor::*;
use crate::ast::*;
use ptree::{print_tree, TreeBuilder};

pub struct AstPrinter {
    tree: TreeBuilder,
}
impl AstPrinter {
    pub fn new() -> AstPrinter {
        AstPrinter {tree: TreeBuilder::new("program".to_string())}
    }

    pub fn print_ast(&mut self, program: &Program) {
        self.visit_program(program)
    }
}

impl AstVisitor for AstPrinter {
    fn visit_program(&mut self, p: &Program) {
        walk_program(self, p);
        self.tree.end_child();
        let top_item = self.tree.build();
        print_tree(&top_item).unwrap()
    }

    fn visit_stmt(&mut self, s: &Stmt) {
        self.tree.begin_child("stmt".to_string());
        walk_stmt(self, s);
        self.tree.end_child();
    }

    fn visit_expr(&mut self, e: &Expr) {
        self.tree.begin_child("expr".to_string());
        walk_expr(self, e);
        self.tree.end_child();
    }

    fn visit_if_block(&mut self, i: &IfBlock) {
        self.tree.begin_child("if_block".to_string());
        walk_if_block(self, i);
        self.tree.end_child();
    }

    fn visit_call_expr(&mut self, c: &CallExpr) {
        self.tree.begin_child("call_expr".to_string());
        walk_call_expr(self, c);
        self.tree.end_child();
    }

    fn visit_else_tail(&mut self, e: &ElseTail) {
        self.tree.begin_child("else_tail".to_string());
        walk_else_tail(self, e);
        self.tree.end_child();
    }

    fn visit_else_block(&mut self, e: &ElseBlock) {
        self.tree.begin_child("else_block".to_string());
        walk_else_block(self, e);
        self.tree.end_child();
    }

    fn visit_fun_def(&mut self, f: Rc<FunDef>) {
        self.tree.begin_child("fun_def".to_string());
        walk_fun_def(self, f);
        self.tree.end_child();
    }

    fn visit_for_loop(&mut self, f: &ForLoopBlock) {
        self.tree.begin_child("for_loop_block".to_string());
        walk_for_loop_block(self, f);
        self.tree.end_child();
    }

    fn visit_param(&mut self, p: &Param) {
        self.tree.begin_child("param".to_string());
        walk_param(self, p);
        self.tree.end_child();
    }

    fn visit_arg(&mut self, a: &Arg) {
        self.tree.begin_child("arg".to_string());
        walk_arg(self, a);
        self.tree.end_child();
    }

    fn visit_range_expr(&mut self, r: &RangeExpr) {
        self.tree.begin_child("range_expr".to_string());
        walk_range_expr(self, r);
        self.tree.end_child();
    }

    fn visit_int_lit(&mut self, i: i32) {
        self.tree.begin_child(format!("{}", i));
        self.tree.end_child();
    }

    fn visit_str_lit(&mut self, s: &String) {
        self.tree.begin_child(format!("{}", s));
        self.tree.end_child();
    }

    fn visit_float_lit(&mut self, f: f64) {
        self.tree.begin_child(format!("{}", f));
        self.tree.end_child();
    }

    fn visit_bool_lit(&mut self, b: bool) {
        self.tree.begin_child(format!("{}", b));
        self.tree.end_child();
    }

    fn visit_name(&mut self, n: &Name) {
        self.tree.begin_child(format!("{:?}", n));
        self.tree.end_child();
    }

    fn visit_bin_op(&mut self, o: BinOp) {
        self.tree.begin_child(format!("{:?}", o));
        self.tree.end_child();
    }

    fn visit_un_op(&mut self, u: UnOp) {
        self.tree.begin_child(format!("{:?}", u));
        self.tree.end_child();
    }

    fn visit_type(&mut self, t: &Type) {
        self.tree.begin_child(format!("{:?}", t));
        self.tree.end_child();
    }
}

