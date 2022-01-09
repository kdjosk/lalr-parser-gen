use std::panic;

use crate::ast::*;
use parser::parse_tree::{*, SyntaxFunction as SynFunc, ParseTreeNode as Node, LexicalValue as LexVal};

pub struct ParseTreeToAst {

}

impl ParseTreeToAst {
    pub fn new() -> ParseTreeToAst {
        ParseTreeToAst {}
    }

    fn process_stmt_seq(&self, node: &NonterminalNode) -> Vec<Stmt> {
        let stmt_seq = Vec::new();
        for child in node.children_ref() {
            match child {
                Node::Internal(node) => stmt_seq.push(self.process_stmt(node)),
                Node::Leaf(_) => panic!(),
            }
        }
        stmt_seq
    }

    fn process_stmt(&self, node: &NonterminalNode) -> Stmt {
        match node.syntax_function() {
            SynFunc::ExprStmt => Stmt::Expr(self.process_expr_stmt(node)),
            SynFunc::AssignmentStmt => self.process_assignment_stmt(node),
            SynFunc::IfStmt => (),
            SynFunc::VarDeclStmt => (),
            SynFunc::FunDefStmt => Stmt::FunDef(self.process_fun_def_stmt(node)),
            SynFunc::ForLoopStmt => (),
            SynFunc::ReturnStmt => (),
            _ => panic!(),
        }
    }

    fn process_if_stmt(&self, node: &NonterminalNode) -> Stmt {
        // ifStmt -> If expr LBrace stmtSeq RBrace elseTail
        // elseTail -> | Else ifStmt | Else LBrace stmtSeq RBrace
        let children = node.children_ref();
        match &children[..] {
            [
                Node::Leaf(_),
                Node::Internal(expr),
                Node::Leaf(_),
                Node::Internal(stmt_seq),
                Node::Leaf(_),
                Node::Internal(else_tail),
            ] => {
                Stmt::If(self.process_expr(expr), )
            }
            _ => panic!(),
        }
    }

    fn process_expr_stmt(&self, node: &NonterminalNode) -> Expr {
        // exprStmt -> expr Semi
        let children = node.children_ref();
        match &children[..] {
            [
                Node::Internal(expr),
                Node::Leaf(_)
            ] => {
                self.process_expr(expr)
            }
            _ => panic!(),
        }
    }

    fn process_assignment_stmt(&self, node: &NonterminalNode) -> Stmt {
        // assignmentStmt -> Identifier Assign exprStmt
        let children = node.children_ref();
        match &children[..] {
            [
                Node::Leaf(id),
                Node::Leaf(_),
                Node::Internal(expr_stmt)
            ] => {
                Stmt::Assignment(self.process_id(id), self.process_expr_stmt(node))
            }
            _ => panic!(),
        }
    }

    fn process_expr(&self, node: &NonterminalNode) -> Expr {
        let children = node.children_ref();
        match &children[..] {
            [Node::Internal(n)] => {
                match n.syntax_function() {
                    SynFunc::Disjunction => self.process_disjunction(n),
                    _ => panic!(),
                }
            }
            _ => panic!(),
        }
    }

    fn process_disjunction(&self, node: &NonterminalNode) -> Expr {
        // disjunction -> conjunction Or disjunction
        let children = node.children_ref();
        match &children[..] {
            [Node::Internal(n)] => {
                self.process_conjunction(n)
            }
            [Node::Internal(lhs), 
             Node::Leaf(_), 
             Node::Internal(rhs)] => {
                Expr::LogicOr(
                    Box::new(self.process_conjunction(lhs)),
                    Box::new(self.process_disjunction(rhs)),
                )
            }
            _ => panic!(),
        }
    }

    fn process_conjunction(&self, node: &NonterminalNode) -> Expr {
        // conjunction -> inversion And conjunction
        let children = node.children_ref();
        match &children[..] {
            [Node::Internal(n)] => {
                self.process_inversion(n)
            }
            [Node::Internal(lhs), 
             Node::Leaf(_), 
             Node::Internal(rhs)] => {
                Expr::LogicAnd(
                    Box::new(self.process_inversion(lhs)),
                    Box::new(self.process_conjunction(rhs)),
                )
            }
            _ => panic!(),
        }
    }

    fn process_inversion(&self, node: &NonterminalNode) -> Expr {
        let children = node.children_ref();
        match &children[..] {
            [Node::Internal(n)] => {
                self.process_comparison(n)
            }
            [Node::Leaf(_), 
             Node::Internal(n)] => {
                Expr::LogicNot(
                    Box::new(self.process_comparison(n))
                )
            }
            _ => panic!(),
        }
    }

    fn process_comparison(&self, node: &NonterminalNode) -> Expr {
        let children = node.children_ref();
        match &children[..] {
            [Node::Internal(n)] => {
                self.process_sum(n)
            }
            [Node::Internal(lhs),
             Node::Leaf(op), 
             Node::Internal(rhs)] => {
                let lhs = Box::new(self.process_sum(lhs));
                let rhs = Box::new(self.process_sum(rhs));
                match op.syntax_function() {
                    SynFunc::Greater => Expr::Greater(lhs, rhs),
                    SynFunc::GreaterEqual => Expr::GreaterEq(lhs, rhs),
                    SynFunc::Less => Expr::Less(lhs, rhs),
                    SynFunc::LessEqual => Expr::LessEq(lhs, rhs),
                    SynFunc::Equal => Expr::Eq(lhs, rhs),
                    _ => panic!(),
                }
            }
            _ => panic!(),
        }
    }

    fn process_sum(&self, node: &NonterminalNode) -> Expr {
        let children = node.children_ref();
        // sum -> sum addOperator term
        match &children[..] {
            [Node::Internal(n)] => {
                self.process_term(n)
            }
            [Node::Internal(lhs), 
             Node::Leaf(op), 
             Node::Internal(rhs)] => {
                let lhs = Box::new(self.process_sum(lhs));
                let rhs = Box::new(self.process_term(rhs));
                match op.syntax_function() {
                    SynFunc::Plus => Expr::Add(lhs, rhs),
                    SynFunc::Minus => Expr::Sub(lhs, rhs),
                    _ => panic!(),
                }
            }
            _ => panic!(),
        }
    }

    fn process_term(&self, node: &NonterminalNode) -> Expr {
        let children = node.children_ref();
        // term -> term multOperator factor
        match &children[..] {
            [Node::Internal(n)] => {
                self.process_factor(n)
            }
            [Node::Internal(lhs), 
             Node::Leaf(op), 
             Node::Internal(rhs)] => {
                let lhs = Box::new(self.process_term(lhs));
                let rhs = Box::new(self.process_factor(rhs));
                match op.syntax_function() {
                    SynFunc::Div => Expr::Div(lhs, rhs),
                    SynFunc::Star => Expr::Mult(lhs, rhs),
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
            [Node::Internal(n)] => {
                self.process_primary(n)
            }
            [Node::Leaf(op), 
             Node::Internal(n)] => {
                let expr = Box::new(self.process_primary(n));
                match op.syntax_function() {
                    SynFunc::Minus => Expr::UnaryMinus(expr),
                    _ => panic!(),
                }
            }
            _ => panic!(),
        }
    }

    fn process_primary(&self, node: &NonterminalNode) -> Expr {
        // primary -> callExpr | atom
        let child = &node.children_ref()[0];
        match child {
            Node::Internal(n) => {
                match n.syntax_function() {
                    SynFunc::CallExpr => self.process_call_expr(n),
                    SynFunc::Atom => self.process_atom(n),
                    _ => panic!(),
                }
            }
            _ => panic!(),
        }
    }

    fn process_call_expr(&self, node: &NonterminalNode) -> Expr {
        
        let children = node.children_ref();
        // callExpr -> Identifier LParen argSeq RParen
        match &children[..] {
            [Node::Leaf(id), 
             Node::Leaf(_),
             Node::Internal(arg_seq),
             Node::Leaf(_),
             ] => {
                Expr::Call(
                    CallExpr::new(
                        self.process_id(id),
                        self.process_arg_seq(arg_seq),
                    )
                )
            }
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
            [Node::Internal(arg),
             Node::Internal(arg_seq_tail)] => {
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
            [Node::Leaf(_),
             Node::Internal(arg),
             Node::Internal(arg_seq_tail)] => {
                args.push(self.process_arg(arg));
                args.extend(self.process_arg_seq_tail(arg_seq_tail));
            }
            _ => panic!(),
        }
        args
    }

    fn process_arg(&self, node: &NonterminalNode) -> Arg {
        // arg -> expr | Identifier Assign expr
        let mut children = node.children_ref();
        match &children[..] {
            [Node::Internal(expr)] => {
                Arg::new(None, self.process_expr(expr))
            }
            [Node::Leaf(id),
             Node::Leaf(_),
             Node::Internal(expr)] => {
                Arg::new(Some(self.process_id(id)), self.process_expr(expr))
            }
            _ => panic!(),
        }
    }

    fn process_atom(&self, node: &NonterminalNode) -> Expr {
        // atom -> Identifier | literal
        let mut children = node.children_ref();
        match &children[..] {
            [Node::Internal(lit)] => {
                self.process_lit(lit)
            }
            [Node::Leaf(id)] => {
                Expr::Identifier(self.process_id(id))
            }
            _ => panic!(),
        }
    }

    fn process_lit(&self, node: &NonterminalNode) -> Expr {
        // literal -> IntegerLiteral | FloatingLiteral | StringLiteral | booleanLiteral
        let mut child = &node.children_ref()[0];
        match child {
            Node::Leaf(n) => {
                match n.lexical_value() {
                    LexVal::IntegerType(i) => Expr::IntLit(i),
                    LexVal::FloatingType(f) => Expr::FloatLit(f),
                    LexVal::StringType(s) => Expr::StrLit(s),
                    _ => panic!(),
                }
            }
            Node::Internal(lit) => {
                self.process_boolean_lit(lit)
            }
            _ => panic!(),
        }
    }

    fn process_boolean_lit(&self, node: &NonterminalNode) -> Expr {
        let mut child = &node.children_ref()[0];
        match child {
            Node::Leaf(n) => {
                match n.syntax_function() {
                    SynFunc::True => Expr::BoolLit(true),
                    SynFunc::False => Expr::BoolLit(false),
                    _ => panic!(),
                }
            }
            _ => panic!(),
        }
    }

    fn process_fun_def_stmt(&self, node: &NonterminalNode) -> FunDef {
        let children = node.children_ref();
        // funDefStmt -> Fn Identifier LParen paramSeq RParen returnDeclaration LBrace stmtSeq RBrace
        match &children[..] {
            [
                Node::Leaf(_), 
                Node::Leaf(id), 
                Node::Leaf(_),
                Node::Internal(params),
                Node::Leaf(_),
                Node::Internal(ret_decl),
                Node::Leaf(_),
                Node::Internal(stmt_seq),
                Node::Leaf(_),
            ] => {
                FunDef::new(
                    self.process_id(id),
                    self.process_params(params),
                    self.process_return_decl(ret_decl),
                    self.process_stmt_seq(stmt_seq),
                )
            },
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

    fn process_params(&self, node: &NonterminalNode) -> Vec<Param> {
        let mut params = Vec::new();
        for param in node.children_ref() {
            match param {
                Node::Internal(n) => params.push(self.process_param(n)),
                _ => panic!(),
            };
        }
        params
    }

    fn process_return_decl(&self, node: &NonterminalNode) -> Type {
        let children = node.children_ref();
        // returnDeclaration -> RArrow typeSpecifier
        match &children[..] {
            [Node::Leaf(_), Node::Internal(type_spec)] => {
                self.process_type_specifier(type_spec)
            }
            _ => panic!(),
        }
    }

    fn process_param(&self, node: &NonterminalNode) -> Param {
        // param -> Identifier Colon typeSpecifier
        let children = node.children_ref();
        match &children[..] {
            [Node::Leaf(id), 
             Node::Leaf(_),
             Node::Internal(type_spec)] => {
                Param::new(
                    self.process_id(id),
                    self.process_type_specifier(type_spec),
                )
            }
            _ => panic!(),
        }
    }

    fn process_type_specifier(&self, node: &NonterminalNode) -> Type {
        let child = &node.children_ref()[0];
        match child {
            Node::Leaf(t) => {
                match t.syntax_function() {
                    SynFunc::U32 => Type::Uint32,
                    SynFunc::U64 => Type::Uint64,
                    SynFunc::F32 => Type::Float32,
                    SynFunc::F64 => Type::Float64,
                    SynFunc::I32 => Type::Int32,
                    SynFunc::I64 => Type::Int64,
                    SynFunc::U8 => Type::Uint8,
                    SynFunc::Bool => Type::Bool,
                    SynFunc::StringType => Type::String,
                    SynFunc::Identifier => {
                        match t.lexical_value() {
                            LexVal::IdentifierType(i) => {
                                Type::UserDefined(Name::new(i))
                            }
                            _ => panic!()
                        }
                    }
                    _ => panic!(),
                }
            }
            _ => panic!()
        }
    }


}
