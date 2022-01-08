use crate::ast::program::Program;
use parser::parse_tree::*;

pub trait ParseTreeToAst {
    fn get_ast(&self, parse_tree_root: Box<dyn Labeled>) -> Program;
}

struct DflowParseTreeToAst {}

impl DflowParseTreeToAst {
    fn visit_stmt_seq(stmt_seq: StmtSeq) {

    }
}

impl ParseTreeToAst for DflowParseTreeToAst {
    fn get_ast(&self, parse_tree_root: Box<dyn Labeled>) -> Program {
        Program::new()
    }
}
