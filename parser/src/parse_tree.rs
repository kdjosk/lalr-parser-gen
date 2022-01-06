use std::{any::Any, fmt::Display};

// TODO(kjoskowi) This whole file should be automatically generated from grammar description

pub trait Node {
    fn get_label(&self) -> String;
    fn get_children(&self) -> Option<&Vec<Box<dyn Node>>>;
}

impl Node for Nonterminal {
    fn get_label(&self) -> String {
        self.function.get_name()
    }
    fn get_children(&self) -> Option<&Vec<Box<dyn Node>>> {
        Some(&self.children)
    }
}
pub struct Nonterminal {
    function: Box<dyn SyntaxFunction>,
    children: Vec<Box<dyn Node>>,
}
impl Nonterminal {
    pub fn new(operation: Box<dyn SyntaxFunction>, children: Vec<Box<dyn Node>>) -> Nonterminal {
        Nonterminal {
            function: operation,
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


pub struct Terminal {
    function: Box<dyn SyntaxFunction>,
    lexical_value: Option<Box<dyn Display>>,
}
impl Node for Terminal {
    fn get_label(&self) -> String {
        let fun = self.function.get_name();
        match &self.lexical_value {
            Some(l) => {
                format!("{} = {}", fun, l)
            }
            None => fun,
        }
    }

    fn get_children(&self) -> Option<&Vec<Box<dyn Node>>> {
        None
    }
}
impl Terminal{
    pub fn new(function: Box<dyn SyntaxFunction>, lexical_value: Option<Box<dyn Display>>) -> Terminal {
        Terminal {
            function,
            lexical_value,
        }
    }
}

pub trait SyntaxFunction {
    fn get_name(&self) -> String;
}

pub struct Program {}
impl SyntaxFunction for Program {
    fn get_name(&self) -> String {
        return "program".to_string()
    }
}

pub struct StmtSeq {}
impl SyntaxFunction for StmtSeq {
    fn get_name(&self) -> String {
        return "StmtSeq".to_string()
    }
}
pub struct Stmt {}
impl SyntaxFunction for Stmt {
    fn get_name(&self) -> String {
        return "Stmt".to_string()
    }
}

pub struct ExprStmt {}
impl SyntaxFunction for ExprStmt {
    fn get_name(&self) -> String {
        return "ExprStmt".to_string()
    }
}
pub struct AssignmentStmt {}
impl SyntaxFunction for AssignmentStmt {
    fn get_name(&self) -> String {
        return "AssignmentStmt".to_string()
    }
}

pub struct IfStmt {}
impl SyntaxFunction for IfStmt {
    fn get_name(&self) -> String {
        return "IfStmt".to_string()
    }
}

pub struct ElseTail {}
impl SyntaxFunction for ElseTail {
    fn get_name(&self) -> String {
        return "ElseTail".to_string()
    }
}

pub struct VarDeclStmt {}
impl SyntaxFunction for VarDeclStmt {
    fn get_name(&self) -> String {
        return "VarDeclStmt".to_string()
    }
}

pub struct FunDefStmt {}
impl SyntaxFunction for FunDefStmt {
    fn get_name(&self) -> String {
        return "FunDefStmt".to_string()
    }
}
pub struct ParamSeq {}
impl SyntaxFunction for ParamSeq {
    fn get_name(&self) -> String {
        return "ParamSeq".to_string()
    }
}
pub struct ParamSeqTail {}
impl SyntaxFunction for ParamSeqTail {
    fn get_name(&self) -> String {
        return "ParamSeqTail".to_string()
    }
}
pub struct Param {}
impl SyntaxFunction for Param {
    fn get_name(&self) -> String {
        return "Param".to_string()
    }
}


pub struct ReturnDeclaration {}
impl SyntaxFunction for ReturnDeclaration {
    fn get_name(&self) -> String {
        return "ReturnDeclaration".to_string()
    }
}
pub struct TypeSpecifier {}
impl SyntaxFunction for TypeSpecifier {
    fn get_name(&self) -> String {
        return "TypeSpecifier".to_string()
    }
}
pub struct PrimitiveTypeSpecifier {}
impl SyntaxFunction for PrimitiveTypeSpecifier {
    fn get_name(&self) -> String {
        return "PrimitiveTypeSpecifier".to_string()
    }
}

pub struct ForLoopStmt {}
impl SyntaxFunction for ForLoopStmt {
    fn get_name(&self) -> String {
        return "VarDeclStmt".to_string()
    }
}

pub struct Let {}
impl SyntaxFunction for Let {
    fn get_name(&self) -> String {
        return "Let".to_string()
    }
}

pub struct For {}
impl SyntaxFunction for For {
    fn get_name(&self) -> String {
        return "For".to_string()
    }
}

pub struct In {}
impl SyntaxFunction for In {
    fn get_name(&self) -> String {
        return "Inl".to_string()
    }
}

pub struct Expr {}
impl SyntaxFunction for Expr {
    fn get_name(&self) -> String {
        return "Expr".to_string()
    }
}

pub struct Disjunction {}
impl SyntaxFunction for Disjunction {
    fn get_name(&self) -> String {
        return "Disjunction".to_string()
    }
}

pub struct ConjunctionSeq {}
impl SyntaxFunction for ConjunctionSeq {
    fn get_name(&self) -> String {
        return "ConjunctionSeq".to_string()
    }
}

pub struct Conjunction {}
impl SyntaxFunction for Conjunction {
    fn get_name(&self) -> String {
        return "Conjunction".to_string()
    }
}
pub struct InversionSeq {}
impl SyntaxFunction for InversionSeq {
    fn get_name(&self) -> String {
        return "InversionSeq".to_string()
    }
}

pub struct Inversion {}
impl SyntaxFunction for Inversion {
    fn get_name(&self) -> String {
        return "Inversion".to_string()
    }
}

pub struct Comparison {}
impl SyntaxFunction for Comparison {
    fn get_name(&self) -> String {
        return "Comparison".to_string()
    }
}

pub struct Sum {}
impl SyntaxFunction for Sum {
    fn get_name(&self) -> String {
        return "Sum".to_string()
    }
}

pub struct Term {}
impl SyntaxFunction for Term {
    fn get_name(&self) -> String {
        return "Term".to_string()
    }
}

pub struct Factor {}
impl SyntaxFunction for Factor {
    fn get_name(&self) -> String {
        return "Factor".to_string()
    }
}

pub struct CallExpr {}
impl SyntaxFunction for CallExpr {
    fn get_name(&self) -> String {
        return "CallExpr".to_string()
    }
}

pub struct Fn {}
impl SyntaxFunction for Fn {
    fn get_name(&self) -> String {
        return "Fn".to_string()
    }
}

pub struct ArgSeq {}
impl SyntaxFunction for ArgSeq {
    fn get_name(&self) -> String {
        return "ArgSeq".to_string()
    }
}

pub struct ArgSeqTail {}
impl SyntaxFunction for ArgSeqTail {
    fn get_name(&self) -> String {
        return "ArgSeqTail".to_string()
    }
}

pub struct Arg {}
impl SyntaxFunction for Arg {
    fn get_name(&self) -> String {
        return "Arg".to_string()
    }
}

pub struct Primary {}
impl SyntaxFunction for Primary {
    fn get_name(&self) -> String {
        return "Primary".to_string()
    }
}

pub struct Atom {}
impl SyntaxFunction for Atom {
    fn get_name(&self) -> String {
        return "Atom".to_string()
    }
}

pub struct RelOperator {}
impl SyntaxFunction for RelOperator {
    fn get_name(&self) -> String {
        return "RelOperator".to_string()
    }
}

pub struct MultOperator {}
impl SyntaxFunction for MultOperator {
    fn get_name(&self) -> String {
        return "MultOperator".to_string()
    }
}

pub struct AddOperator {}
impl SyntaxFunction for AddOperator {
    fn get_name(&self) -> String {
        return "AddOperator".to_string()
    }
}
pub struct Literal {}
impl SyntaxFunction for Literal {
    fn get_name(&self) -> String {
        return "Literal".to_string()
    }
}

pub struct BooleanLiteral {}
impl SyntaxFunction for BooleanLiteral {
    fn get_name(&self) -> String {
        return "BooleanLiteral".to_string()
    }
}

pub struct Semi {}
impl SyntaxFunction for Semi {
    fn get_name(&self) -> String {
        return "Semi".to_string()
    }
}

pub struct Coma {}
impl SyntaxFunction for Coma {
    fn get_name(&self) -> String {
        return "Coma".to_string()
    }
}


pub struct Identifier {} 
impl SyntaxFunction for Identifier {
    fn get_name(&self) -> String {
        return "Identifier".to_string()
    }
}
pub struct Assign {} 
impl SyntaxFunction for Assign {
    fn get_name(&self) -> String {
        return "Assign".to_string()
    }
}
pub struct If {}
impl SyntaxFunction for If {
    fn get_name(&self) -> String {
        return "If".to_string()
    }
}

pub struct Else {}
impl SyntaxFunction for Else {
    fn get_name(&self) -> String {
        return "Else".to_string()
    }
}

pub struct LBrace {}
impl SyntaxFunction for LBrace {
    fn get_name(&self) -> String {
        return "LBrace".to_string()
    }
}
pub struct RBrace {}
impl SyntaxFunction for RBrace {
    fn get_name(&self) -> String {
        return "RBrace".to_string()
    }
}

pub struct LParen {}
impl SyntaxFunction for LParen {
    fn get_name(&self) -> String {
        return "LParen".to_string()
    }
}
pub struct RParen {}
impl SyntaxFunction for RParen {
    fn get_name(&self) -> String {
        return "RParen".to_string()
    }
}

pub struct Or {}
impl SyntaxFunction for Or {
    fn get_name(&self) -> String {
        return "Or".to_string()
    }
}
pub struct And {}
impl SyntaxFunction for And {
    fn get_name(&self) -> String {
        return "And".to_string()
    }
}
pub struct Not {} 
impl SyntaxFunction for Not {
    fn get_name(&self) -> String {
        return "Not".to_string()
    }
}
pub struct Less {}
impl SyntaxFunction for Less {
    fn get_name(&self) -> String {
        return "Less".to_string()
    }
}
pub struct Greater {}
impl SyntaxFunction for Greater {
    fn get_name(&self) -> String {
        return "Greater".to_string()
    }
}
pub struct LessEqual {}
impl SyntaxFunction for LessEqual {
    fn get_name(&self) -> String {
        return "LessEqual".to_string()
    }
}
pub struct GreaterEqual {}
impl SyntaxFunction for GreaterEqual {
    fn get_name(&self) -> String {
        return "GreaterEqual".to_string()
    }
}
pub struct Equal {}
impl SyntaxFunction for Equal {
    fn get_name(&self) -> String {
        return "Equal".to_string()
    }
}
pub struct NotEqual {}
impl SyntaxFunction for NotEqual {
    fn get_name(&self) -> String {
        return "NotEqual".to_string()
    }
}
pub struct Star {}
impl SyntaxFunction for Star {
    fn get_name(&self) -> String {
        return "Star".to_string()
    }
}
pub struct Div {}
impl SyntaxFunction for Div {
    fn get_name(&self) -> String {
        return "Div".to_string()
    }
}
pub struct Minus {}
impl SyntaxFunction for Minus {
    fn get_name(&self) -> String {
        return "Minus".to_string()
    }
}
pub struct Plus {} 
impl SyntaxFunction for Plus {
    fn get_name(&self) -> String {
        return "Plus".to_string()
    }
}
pub struct IntegerLiteral {}
impl SyntaxFunction for IntegerLiteral {
    fn get_name(&self) -> String {
        return "IntegerLiteral".to_string()
    }
}
pub struct FloatingLiteral {}
impl SyntaxFunction for FloatingLiteral {
    fn get_name(&self) -> String {
        return "FloatingLiteral".to_string()
    }
}
pub struct StringLiteral {}
impl SyntaxFunction for StringLiteral {
    fn get_name(&self) -> String {
        return "StringLiteral".to_string()
    }
}
pub struct True {}
impl SyntaxFunction for True {
    fn get_name(&self) -> String {
        return "True".to_string()
    }
}
pub struct False {}
impl SyntaxFunction for False {
    fn get_name(&self) -> String {
        return "False".to_string()
    }
}

pub struct RightArrow {}
impl SyntaxFunction for RightArrow {
    fn get_name(&self) -> String {
        return "RightArrow".to_string()
    }
}

pub struct Colon {}
impl SyntaxFunction for Colon {
    fn get_name(&self) -> String {
        return "Colon".to_string()
    }
}

pub struct UnaryOperator {}
impl SyntaxFunction for UnaryOperator {
    fn get_name(&self) -> String {
        return "UnaryOperator".to_string()
    }
}


pub struct U32 {}
impl SyntaxFunction for U32 {
    fn get_name(&self) -> String {
        return "U32".to_string()
    }
}
pub struct I32 {}
impl SyntaxFunction for I32 {
    fn get_name(&self) -> String {
        return "I32".to_string()
    }
}
pub struct F32 {}
impl SyntaxFunction for F32 {
    fn get_name(&self) -> String {
        return "F32".to_string()
    }
}
pub struct U64 {}
impl SyntaxFunction for U64 {
    fn get_name(&self) -> String {
        return "U64".to_string()
    }
}
pub struct I64 {}
impl SyntaxFunction for I64 {
    fn get_name(&self) -> String {
        return "I64".to_string()
    }
}
pub struct F64 {}
impl SyntaxFunction for F64 {
    fn get_name(&self) -> String {
        return "F64".to_string()
    }
}
pub struct U8 {}
impl SyntaxFunction for U8 {
    fn get_name(&self) -> String {
        return "U8".to_string()
    }
}
pub struct Bool {}
impl SyntaxFunction for Bool {
    fn get_name(&self) -> String {
        return "Bool".to_string()
    }
}
pub struct StringType {}
impl SyntaxFunction for StringType {
    fn get_name(&self) -> String {
        return "StringType".to_string()
    }
}





