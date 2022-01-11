use std::panic;

use crate::ast::*;
use parser::parse_tree::{
    LexicalValue as LexVal, ParseTreeNode as Node, SyntaxFunction as SynFunc, *,
};

pub trait ParseTreeToAst {
    fn get_ast(&self, root_node: &ParseTreeNode) -> Program;
}

pub struct DflowParseTreeToAst;
impl ParseTreeToAst for DflowParseTreeToAst {
    fn get_ast(&self, root_node: &ParseTreeNode) -> Program {
        match root_node {
            Node::Internal(n) => {
                Program::new(self.process_stmt_seq(n))
            }
            _ => panic!(),
        }
    }
}

impl DflowParseTreeToAst {
    pub fn new() -> DflowParseTreeToAst {
        DflowParseTreeToAst {}
    }

    fn process_stmt_seq(&self, node: &NonterminalNode) -> Vec<Stmt> {
        // stmtSeq -> stmtSeq stmt | stmt
        let mut stmt_seq = Vec::new();
        let children = node.children_ref();
        match &children[..] {
            [Node::Internal(stmt)] => stmt_seq.push(self.process_stmt(stmt)),
            [Node::Internal(seq),
             Node::Internal(stmt),] => {
                stmt_seq.extend(self.process_stmt_seq(seq));
                stmt_seq.push(self.process_stmt(stmt));
            } 
            _ => panic!(),
        }
        stmt_seq
    }

    fn process_stmt(&self, node: &NonterminalNode) -> Stmt {
        let child = &node.children_ref()[0];
        match child {
            Node::Internal(n) => {
                match n.syntax_function() {
                    SynFunc::ExprStmt => Stmt::Expr(self.process_expr_stmt(n)),
                    SynFunc::AssignmentStmt => {
                        let (name, expr) = self.process_assignment_stmt(n);
                        Stmt::Assignment(name, expr)
                    }
                    SynFunc::IfStmt => Stmt::If(self.process_if_stmt(n)),
                    SynFunc::VarDeclStmt => self.process_var_decl_stmt(n),
                    SynFunc::FunDefStmt => Stmt::FunDef(self.process_fun_def_stmt(n)),
                    SynFunc::ForLoopStmt => Stmt::ForLoop(self.process_for_loop_stmt(n)),
                    SynFunc::ReturnStmt => self.process_return_stmt(n),
                    _ => panic!(),
                }
            }
            _ => panic!(),
        }

    }

    fn process_return_stmt(&self, node: &NonterminalNode) -> Stmt {
        // returnStmt -> Return expr Semi
        let children = node.children_ref();
        match &children[..] {
            [Node::Leaf(_), 
             Node::Internal(expr),
             Node::Leaf(_)] => {
                Stmt::Return(self.process_expr(expr))
            }
            _ => panic!(),
        }
    }

    fn process_for_loop_stmt(&self, node: &NonterminalNode) -> ForLoopBlock {
        // forLoopStmt -> For Identifier In expr LBrace stmtSeq RBrace
        let children = node.children_ref();
        match &children[..] {
            [Node::Leaf(_), 
             Node::Leaf(id),
             Node::Leaf(_),
             Node::Internal(expr),
             Node::Leaf(_),
             Node::Internal(stmt_seq),
             Node::Leaf(_)] => {
                ForLoopBlock::new(
                    self.process_id(id),
                    self.process_expr(expr),
                    self.process_stmt_seq(stmt_seq),
                )
            }
            _ => panic!(),
        }
    } 

    fn process_var_decl_stmt(&self, node: &NonterminalNode) -> Stmt {
        // varDeclStmt -> Let assignmentStmt
        let children = node.children_ref();
        match &children[..] {
            [Node::Leaf(_), 
             Node::Internal(assign_stmt)] => {
                let (n, e) = self.process_assignment_stmt(assign_stmt);
                Stmt::VarDecl(n, e)
            }
            _ => panic!(),
        }
    }

    fn process_if_stmt(&self, node: &NonterminalNode) -> IfBlock {
        // ifStmt -> If expr LBrace stmtSeq RBrace elseTail
        let children = node.children_ref();
        match &children[..] {
            [Node::Leaf(_), 
             Node::Internal(expr),
             Node::Leaf(_), 
             Node::Internal(stmt_seq), 
             Node::Leaf(_), 
             Node::Internal(else_tail)] => {
                IfBlock::new(
                    self.process_expr(expr),
                    self.process_stmt_seq(stmt_seq),
                    self.process_else_tail(else_tail),
                )
            }
            _ => panic!(),
        }
    }

    fn process_else_tail(&self, node: &NonterminalNode) -> Option<ElseTail> {
        // elseTail -> | Else ifStmt | Else LBrace stmtSeq RBrace
        let children = node.children_ref();
        match &children[..] {
            [] => None,
            [Node::Leaf(_), Node::Internal(if_stmt)] => Some(ElseTail::new(
                Some(Box::new(self.process_if_stmt(if_stmt))),
                None,
            )),
            [Node::Leaf(_), Node::Leaf(_), Node::Internal(stmt_seq), Node::Leaf(_)] => Some(
                ElseTail::new(None, Some(ElseBlock::new(self.process_stmt_seq(stmt_seq)))),
            ),
            _ => panic!(),
        }
    }

    fn process_expr_stmt(&self, node: &NonterminalNode) -> Expr {
        // exprStmt -> expr Semi
        let children = node.children_ref();
        match &children[..] {
            [Node::Internal(expr), Node::Leaf(_)] => self.process_expr(expr),
            _ => panic!(),
        }
    }

    fn process_assignment_stmt(&self, node: &NonterminalNode) -> (Name, Expr) {
        // assignmentStmt -> Identifier Assign exprStmt
        let children = node.children_ref();
        match &children[..] {
            [Node::Leaf(id), Node::Leaf(_), Node::Internal(expr_stmt)] => {
                (self.process_id(id), self.process_expr_stmt(expr_stmt))
            }
            _ => panic!(),
        }
    }

    fn process_expr(&self, node: &NonterminalNode) -> Expr {
        let children = node.children_ref();
        match &children[..] {
            [Node::Internal(n)] => match n.syntax_function() {
                SynFunc::Disjunction => self.process_disjunction(n),
                _ => panic!(),
            },
            _ => panic!(),
        }
    }

    fn process_disjunction(&self, node: &NonterminalNode) -> Expr {
        // disjunction -> conjunction Or disjunction
        let children = node.children_ref();
        match &children[..] {
            [Node::Internal(n)] => self.process_conjunction(n),
            [Node::Internal(lhs), Node::Leaf(_), Node::Internal(rhs)] => Expr::Binary(
                BinOp::LogicOr,
                Box::new(self.process_conjunction(lhs)),
                Box::new(self.process_disjunction(rhs)),
            ),
            _ => panic!(),
        }
    }

    fn process_conjunction(&self, node: &NonterminalNode) -> Expr {
        // conjunction -> inversion And conjunction
        let children = node.children_ref();
        match &children[..] {
            [Node::Internal(n)] => self.process_inversion(n),
            [Node::Internal(lhs), Node::Leaf(_), Node::Internal(rhs)] => Expr::Binary(
                BinOp::LogicAnd,
                Box::new(self.process_inversion(lhs)),
                Box::new(self.process_conjunction(rhs)),
            ),
            _ => panic!(),
        }
    }

    fn process_inversion(&self, node: &NonterminalNode) -> Expr {
        let children = node.children_ref();
        match &children[..] {
            [Node::Internal(n)] => self.process_comparison(n),
            [Node::Leaf(_), Node::Internal(n)] => {
                Expr::Unary(UnOp::Not, Box::new(self.process_comparison(n)))
            }
            _ => panic!(),
        }
    }

    fn process_comparison(&self, node: &NonterminalNode) -> Expr {
        // comparison -> sum relOperator sum | sum
        let children = node.children_ref();
        match &children[..] {
            [Node::Internal(n)] => self.process_sum(n),
            [Node::Internal(lhs), Node::Internal(op), Node::Internal(rhs)] => {
                let lhs = Box::new(self.process_sum(lhs));
                let rhs = Box::new(self.process_sum(rhs));
                match self.process_op(op) {
                    SynFunc::Greater => Expr::Binary(BinOp::Greater, lhs, rhs),
                    SynFunc::GreaterEqual => Expr::Binary(BinOp::GreaterEq, lhs, rhs),
                    SynFunc::Less => Expr::Binary(BinOp::Less, lhs, rhs),
                    SynFunc::LessEqual => Expr::Binary(BinOp::LessEq, lhs, rhs),
                    SynFunc::Equal => Expr::Binary(BinOp::Eq, lhs, rhs),
                    _ => panic!(),
                }
            }
            _ => panic!(),
        }
    }

    fn process_sum(&self, node: &NonterminalNode) -> Expr {
        let children = node.children_ref();
        // sum -> sum addOperator term | term
        match &children[..] {
            [Node::Internal(n)] => self.process_term(n),
            [Node::Internal(lhs), Node::Internal(op), Node::Internal(rhs)] => {
                let lhs = Box::new(self.process_sum(lhs));
                let rhs = Box::new(self.process_term(rhs));
                match self.process_op(op){
                    SynFunc::Plus => Expr::Binary(BinOp::Add, lhs, rhs),
                    SynFunc::Minus => Expr::Binary(BinOp::Sub, lhs, rhs),
                    _ => panic!(),
                }
            }
            _ => panic!(),
        }
    }

    fn process_term(&self, node: &NonterminalNode) -> Expr {
        let children = node.children_ref();
        // term -> term multOperator factor | factor
        match &children[..] {
            [Node::Internal(n)] => self.process_factor(n),
            [Node::Internal(lhs), Node::Internal(op), Node::Internal(rhs)] => {
                let lhs = Box::new(self.process_term(lhs));
                let rhs = Box::new(self.process_factor(rhs));
                match self.process_op(op) {
                    SynFunc::Div => Expr::Binary(BinOp::Div, lhs, rhs),
                    SynFunc::Star => Expr::Binary(BinOp::Mult, lhs, rhs),
                    _ => panic!(),
                }
            }
            _ => panic!(),
        }
    }

    fn process_factor(&self, node: &NonterminalNode) -> Expr {
        let children = node.children_ref();
        // factor -> primary | unaryOperator primary
        match &children[..] {
            [Node::Internal(n)] => self.process_primary(n),
            [Node::Internal(op), Node::Internal(n)] => {
                let expr = Box::new(self.process_primary(n));
                match self.process_op(op) {
                    SynFunc::Minus => Expr::Unary(UnOp::Minus, expr),
                    _ => panic!(),
                }
            }
            _ => panic!(),
        }
    }

    fn process_op(&self, node: &NonterminalNode) -> SynFunc {
        let child = &node.children_ref()[0];
        match child {
            Node::Leaf(op) => {
                op.syntax_function()
            }
            _ => panic!(),
        }
    }

    fn process_primary(&self, node: &NonterminalNode) -> Expr {
        // primary -> callExpr | atom
        let child = &node.children_ref()[0];
        match child {
            Node::Internal(n) => match n.syntax_function() {
                SynFunc::CallExpr => self.process_call_expr(n),
                SynFunc::Atom => self.process_atom(n),
                _ => panic!(),
            },
            _ => panic!(),
        }
    }

    fn process_call_expr(&self, node: &NonterminalNode) -> Expr {
        let children = node.children_ref();
        // callExpr -> Identifier LParen argSeq RParen
        match &children[..] {
            [Node::Leaf(id), Node::Leaf(_), Node::Internal(arg_seq), Node::Leaf(_)] => Expr::Call(
                CallExpr::new(self.process_id(id), self.process_arg_seq(arg_seq)),
            ),
            _ => panic!(),
        }
    }

    fn process_arg_seq(&self, node: &NonterminalNode) -> Vec<Arg> {
        let mut args = Vec::new();
        let children = node.children_ref();
        // argSeq -> arg argSeqTail |

        match &children[..] {
            [] => (),
            [Node::Internal(n)] => {
                args.push(self.process_arg(n));
            }
            [Node::Internal(arg), Node::Internal(arg_seq_tail)] => {
                args.push(self.process_arg(arg));
                args.extend(self.process_arg_seq_tail(arg_seq_tail));
            }
            _ => panic!(),
        }
        args
    }

    fn process_arg_seq_tail(&self, node: &NonterminalNode) -> Vec<Arg> {
        let mut args = Vec::new();
        let children = node.children_ref();
        // argSeqTail -> Coma arg argSeqTail |
        match &children[..] {
            [] => (),
            [Node::Leaf(_), Node::Internal(arg), Node::Internal(arg_seq_tail)] => {
                args.push(self.process_arg(arg));
                args.extend(self.process_arg_seq_tail(arg_seq_tail));
            }
            _ => panic!(),
        }
        args
    }

    fn process_arg(&self, node: &NonterminalNode) -> Arg {
        // arg -> expr | Identifier Assign expr
        let children = node.children_ref();
        match &children[..] {
            [Node::Internal(expr)] => Arg::new(None, self.process_expr(expr)),
            [Node::Leaf(id), Node::Leaf(_), Node::Internal(expr)] => {
                Arg::new(Some(self.process_id(id)), self.process_expr(expr))
            }
            _ => panic!(),
        }
    }

    fn process_atom(&self, node: &NonterminalNode) -> Expr {
        // atom -> Identifier | literal
        let children = node.children_ref();
        match &children[..] {
            [Node::Internal(lit)] => self.process_lit(lit),
            [Node::Leaf(id)] => Expr::Identifier(self.process_id(id)),
            _ => panic!(),
        }
    }

    fn process_lit(&self, node: &NonterminalNode) -> Expr {
        // literal -> IntegerLiteral | FloatingLiteral | StringLiteral | booleanLiteral
        let child = &node.children_ref()[0];
        match child {
            Node::Leaf(n) => match n.lexical_value() {
                LexVal::IntegerType(i) => Expr::IntLit(i),
                LexVal::FloatingType(f) => Expr::FloatLit(f),
                LexVal::StringType(s) => Expr::StrLit(s),
                _ => panic!(),
            },
            Node::Internal(lit) => self.process_boolean_lit(lit),
        }
    }

    fn process_boolean_lit(&self, node: &NonterminalNode) -> Expr {
        let child = &node.children_ref()[0];
        match child {
            Node::Leaf(n) => match n.syntax_function() {
                SynFunc::True => Expr::BoolLit(true),
                SynFunc::False => Expr::BoolLit(false),
                _ => panic!(),
            },
            _ => panic!(),
        }
    }

    fn process_fun_def_stmt(&self, node: &NonterminalNode) -> FunDef {
        let children = node.children_ref();
        // funDefStmt -> Fn Identifier LParen paramSeq RParen returnDeclaration LBrace stmtSeq RBrace
        match &children[..] {
            [Node::Leaf(_), Node::Leaf(id), Node::Leaf(_), Node::Internal(params), Node::Leaf(_), Node::Internal(ret_decl), Node::Leaf(_), Node::Internal(stmt_seq), Node::Leaf(_)] => {
                FunDef::new(
                    self.process_id(id),
                    self.process_param_seq(params),
                    self.process_return_decl(ret_decl),
                    self.process_stmt_seq(stmt_seq),
                )
            }
            _ => panic!(),
        }
    }

    fn process_id(&self, node: &TerminalNode) -> Name {
        if let LexVal::IdentifierType(i) = node.lexical_value() {
            Name::new(i)
        } else {
            panic!();
        }
    }

    fn process_return_decl(&self, node: &NonterminalNode) -> Type {
        let children = node.children_ref();
        // returnDeclaration -> RArrow typeSpecifier
        match &children[..] {
            [Node::Leaf(_), Node::Internal(type_spec)] => self.process_type_specifier(type_spec),
            _ => panic!(),
        }
    }

    fn process_param_seq(&self, node: &NonterminalNode) -> Vec<Param> {
        // paramSeq -> | param paramSeqTail
        let mut params = Vec::new();
        let children = node.children_ref();
        match &children[..] {
            [] => (),
            [Node::Internal(param), Node::Internal(tail)] => {
                params.push(self.process_param(param));
                params.extend(self.process_param_seq_tail(tail));
            }
            _ => panic!(),
        }
        params
    }

    fn process_param_seq_tail(&self, node: &NonterminalNode) -> Vec<Param> {
        // paramSeqTail -> | Coma param paramSeqTail
        let mut params = Vec::new();
        let children = node.children_ref();
        match &children[..] {
            [] => (),
            [Node::Leaf(_), Node::Internal(param), Node::Internal(tail)] => {
                params.push(self.process_param(param));
                params.extend(self.process_param_seq_tail(tail));
            }
            _ => panic!(),
        }
        params
    }


    fn process_param(&self, node: &NonterminalNode) -> Param {
        // param -> Identifier Colon typeSpecifier
        let children = node.children_ref();
        match &children[..] {
            [Node::Leaf(id), Node::Leaf(_), Node::Internal(type_spec)] => {
                Param::new(self.process_id(id), self.process_type_specifier(type_spec))
            }
            _ => panic!(),
        }
    }

    fn process_type_specifier(&self, node: &NonterminalNode) -> Type {
        let child = &node.children_ref()[0];
        match child {
            Node::Leaf(t) => match t.syntax_function() {
                SynFunc::U32 => Type::Uint32,
                SynFunc::U64 => Type::Uint64,
                SynFunc::F32 => Type::Float32,
                SynFunc::F64 => Type::Float64,
                SynFunc::I32 => Type::Int32,
                SynFunc::I64 => Type::Int64,
                SynFunc::U8 => Type::Uint8,
                SynFunc::Bool => Type::Bool,
                SynFunc::StringType => Type::String,
                SynFunc::Identifier => match t.lexical_value() {
                    LexVal::IdentifierType(i) => Type::UserDefined(Name::new(i)),
                    _ => panic!(),
                },
                _ => panic!(),
            },
            _ => panic!(),
        }
    }
}
