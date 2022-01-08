use std::{any::Any, fmt::{Display, self}};

// TODO(kjoskowi) This whole file should be automatically generated from grammar description
pub trait ParseTreeVisitor {
    fn visit_stmt_seq(&mut self, stmt_seq: NonterminalNode<StmtSeq>);
}

pub enum ParseTreeNode {
    Leaf(Box<dyn TerminalNodeT>),
    Internal(Box<dyn NonterminalNodeT>)
}

pub trait Labeled {
    fn get_label(&self) -> String;
}

pub trait Nonterminal {
    fn get_children_ref(&self) -> &Vec<ParseTreeNode>;
    fn add_child(&mut self, child: ParseTreeNode); 
}

pub trait Terminal {
    fn get_lexical_value(&self) -> LexicalValue;
}


pub trait NonterminalNodeT: Nonterminal + Labeled {}
impl<T> NonterminalNodeT for T where
    T: Nonterminal + Labeled {}
pub trait TerminalNodeT: Terminal + Labeled {}
impl<T>  TerminalNodeT for T where
    T: Terminal + Labeled {}


pub struct NonterminalNode<T> {
    children: Vec<ParseTreeNode>,
    label: String,
    t: T,
}

impl<T: SyntaxFunction> NonterminalNode<T> {
    pub fn new(label: &str, t: T) -> NonterminalNode<T> {
        let label = label.to_string();
        NonterminalNode{
            children: Vec::new(),
            label,
            t,
        }
    }
}
impl<T: SyntaxFunction> Labeled for NonterminalNode<T> {
    fn get_label(&self) -> String {
        self.label.clone()
    }
}
impl<T: SyntaxFunction> Nonterminal for NonterminalNode<T> {
    fn get_children_ref(&self) -> &Vec<ParseTreeNode> {
        &self.children
    }
    fn add_child(&mut self, child: ParseTreeNode) {
        self.children.push(child);
    }
}

#[derive(Clone)]
pub enum LexicalValue {
    StringType(String),
    IntegerType(u64),
    FloatingType(f64),
    BooleanType(bool),
    IdentifierType(String),
    NoLexicalValue,
}
impl Display for LexicalValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LexicalValue::StringType(v) => write!(f, "{}", v),
            LexicalValue::IntegerType(v) => write!(f, "{}", v),
            LexicalValue::FloatingType(v) => write!(f, "{}", v),
            LexicalValue::BooleanType(v) => write!(f, "{}", v),
            LexicalValue::IdentifierType(v) => write!(f, "{}", v),
            LexicalValue::NoLexicalValue => write!(f, ""),
        }
    }
}

pub struct TerminalNode<T> {
    lex_value: LexicalValue,
    label: String,
    t: T,
}
impl<T: SyntaxFunction> TerminalNode<T> {
    pub fn new(label: &str, lex_value: LexicalValue, t: T) -> TerminalNode<T> {
        let label = label.to_string();
        TerminalNode{
            lex_value,
            label,
            t,
        }
    }
}
impl<T: SyntaxFunction> Labeled for TerminalNode<T> {
    fn get_label(&self) -> String {
        format!("{}({})", self.label.clone(), self.lex_value)
    }
}
impl<T: SyntaxFunction> Terminal for TerminalNode<T> {
    fn get_lexical_value(&self) -> LexicalValue {
        self.lex_value.clone()
    }
}

pub trait SyntaxFunction{}
pub struct Program {}
impl SyntaxFunction for Program {}
pub struct StmtSeq {}
impl SyntaxFunction for StmtSeq {}
pub struct Stmt {}
impl SyntaxFunction for Stmt {}
pub struct ExprStmt {}
impl SyntaxFunction for ExprStmt {}
pub struct AssignmentStmt {}
impl SyntaxFunction for AssignmentStmt {}
pub struct IfStmt {}
impl SyntaxFunction for IfStmt {}
pub struct ElseTail {}
impl SyntaxFunction for ElseTail {}
pub struct VarDeclStmt {}
impl SyntaxFunction for VarDeclStmt {}
pub struct FunDefStmt {}
impl SyntaxFunction for FunDefStmt {}
pub struct ParamSeq {}
impl SyntaxFunction for ParamSeq {}
pub struct ParamSeqTail {}
impl SyntaxFunction for ParamSeqTail {}
pub struct Param {}
impl SyntaxFunction for Param {}
pub struct ReturnDeclaration {}
impl SyntaxFunction for ReturnDeclaration {}
pub struct ReturnStmt {}
impl SyntaxFunction for ReturnStmt {}
pub struct TypeSpecifier {}
impl SyntaxFunction for TypeSpecifier {}
pub struct PrimitiveTypeSpecifier {}
impl SyntaxFunction for PrimitiveTypeSpecifier {}
pub struct ForLoopStmt {}
impl SyntaxFunction for ForLoopStmt {}
pub struct Let {}
impl SyntaxFunction for Let {}
pub struct For {}
impl SyntaxFunction for For {}
pub struct In {}
impl SyntaxFunction for In {}
pub struct Expr {}
impl SyntaxFunction for Expr {}
pub struct Disjunction {}
impl SyntaxFunction for Disjunction {}
pub struct ConjunctionSeq {}
impl SyntaxFunction for ConjunctionSeq {}
pub struct Conjunction {}
impl SyntaxFunction for Conjunction {}
pub struct InversionSeq {}
impl SyntaxFunction for InversionSeq {}
pub struct Inversion {}
impl SyntaxFunction for Inversion {}
pub struct Comparison {}
impl SyntaxFunction for Comparison {}
pub struct Sum {}
impl SyntaxFunction for Sum {}
pub struct Term {}
impl SyntaxFunction for Term {}
pub struct Factor {}
impl SyntaxFunction for Factor {}
pub struct CallExpr {}
impl SyntaxFunction for CallExpr {}
pub struct Fun {}
impl SyntaxFunction for Fun {}
pub struct ArgSeq {}
impl SyntaxFunction for ArgSeq {}
pub struct ArgSeqTail {}
impl SyntaxFunction for ArgSeqTail {}
pub struct Arg {}
impl SyntaxFunction for Arg {}
pub struct Primary {}
impl SyntaxFunction for Primary {}
pub struct Atom {}
impl SyntaxFunction for Atom {}
pub struct RelOperator {}
impl SyntaxFunction for RelOperator {}
pub struct MultOperator {}
impl SyntaxFunction for MultOperator {}
pub struct AddOperator {}
impl SyntaxFunction for AddOperator {}
pub struct Literal {}
impl SyntaxFunction for Literal {}
pub struct BooleanLiteral {}
impl SyntaxFunction for BooleanLiteral {}
pub struct Semi {}
impl SyntaxFunction for Semi {}
pub struct Coma {}
impl SyntaxFunction for Coma {}
pub struct Return {}
impl SyntaxFunction for Return {}
pub struct RArrow {}
impl SyntaxFunction for RArrow {}
pub struct Identifier {}
impl SyntaxFunction for Identifier {}
pub struct Assign {}
impl SyntaxFunction for Assign {}
pub struct If {}
impl SyntaxFunction for If {}
pub struct Else {}
impl SyntaxFunction for Else {}
pub struct LBrace {}
impl SyntaxFunction for LBrace {}
pub struct RBrace {}
impl SyntaxFunction for RBrace {}
pub struct LParen {}
impl SyntaxFunction for LParen {}
pub struct RParen {}
impl SyntaxFunction for RParen {}
pub struct Or {}
impl SyntaxFunction for Or {}
pub struct And {}
impl SyntaxFunction for And {}
pub struct Not {}
impl SyntaxFunction for Not {}
pub struct Less {}
impl SyntaxFunction for Less {}
pub struct Greater {}
impl SyntaxFunction for Greater {}
pub struct LessEqual {}
impl SyntaxFunction for LessEqual {}
pub struct GreaterEqual {}
impl SyntaxFunction for GreaterEqual {}
pub struct Equal {}
impl SyntaxFunction for Equal {}
pub struct NotEqual {}
impl SyntaxFunction for NotEqual {}
pub struct Star {}
impl SyntaxFunction for Star {}
pub struct Div {}
impl SyntaxFunction for Div {}
pub struct Minus {}
impl SyntaxFunction for Minus {}
pub struct Plus {}
impl SyntaxFunction for Plus {}
pub struct IntegerLiteral {}
impl SyntaxFunction for IntegerLiteral {}
pub struct FloatingLiteral {}
impl SyntaxFunction for FloatingLiteral {}
pub struct StringLiteral {}
impl SyntaxFunction for StringLiteral {}
pub struct True {}
impl SyntaxFunction for True {}
pub struct False {}
impl SyntaxFunction for False {}
pub struct Colon {}
impl SyntaxFunction for Colon {}
pub struct UnaryOperator {}
impl SyntaxFunction for UnaryOperator {}
pub struct U32 {}
impl SyntaxFunction for U32 {}
pub struct I32 {}
impl SyntaxFunction for I32 {}
pub struct F32 {}
impl SyntaxFunction for F32 {}
pub struct U64 {}
impl SyntaxFunction for U64 {}
pub struct I64 {}
impl SyntaxFunction for I64 {}
pub struct F64 {}
impl SyntaxFunction for F64 {}
pub struct U8 {}
impl SyntaxFunction for U8 {}
pub struct Bool {}
impl SyntaxFunction for Bool {}
pub struct StringType {}
impl SyntaxFunction for StringType {}
