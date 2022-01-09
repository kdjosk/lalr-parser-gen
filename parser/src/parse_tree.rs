use std::{fmt::{Display, self}};

// TODO(kjoskowi) This whole file should be automatically generated from grammar description

pub enum ParseTreeNode {
    Leaf(TerminalNode),
    Internal(NonterminalNode),
}

pub trait Labeled {
    fn get_label(&self) -> String;
}

pub trait Functional {
    fn syntax_function(&self) -> SyntaxFunction;
}


pub trait Nonterminal {
    fn children_ref(&self) -> &Vec<ParseTreeNode>;
    fn add_child(&mut self, child: ParseTreeNode); 
    fn reverse_children(&mut self);
}

pub trait Terminal {
    fn lexical_value(&self) -> LexicalValue;
}

pub trait NonterminalNodeT: Nonterminal + Labeled + Functional {}
pub trait TerminalNodeT: Terminal + Labeled + Functional {}

pub struct NonterminalNode {
    children: Vec<ParseTreeNode>,
    label: String,
    func: SyntaxFunction,
}

impl NonterminalNode {
    pub fn new(label: &str, func: SyntaxFunction) -> NonterminalNode {
        let label = label.to_string();
        NonterminalNode{
            children: Vec::new(),
            label,
            func,
        }
    }
}
impl  NonterminalNodeT for NonterminalNode {}
impl Labeled for NonterminalNode {
    fn get_label(&self) -> String {
        self.label.clone()
    }
}
impl Nonterminal for NonterminalNode {
    fn children_ref(&self) -> &Vec<ParseTreeNode> {
        &self.children
    }
    fn add_child(&mut self, child: ParseTreeNode) {
        self.children.push(child);
    }
    fn reverse_children(&mut self) {
        self.children.reverse();
    }
}
impl Functional for NonterminalNode {
    fn syntax_function(&self) -> SyntaxFunction {
        self.func
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

pub struct TerminalNode {
    lex_value: LexicalValue,
    label: String,
    func: SyntaxFunction,
}
impl TerminalNode {
    pub fn new(label: &str, lex_value: LexicalValue, func: SyntaxFunction) -> TerminalNode {
        let label = label.to_string();
        TerminalNode{
            lex_value,
            label,
            func,
        }
    }
}
impl TerminalNodeT for TerminalNode {}

impl Labeled for TerminalNode {
    fn get_label(&self) -> String {
        format!("{}({})", self.label.clone(), self.lex_value)
    }
}
impl Terminal for TerminalNode {
    fn lexical_value(&self) -> LexicalValue {
        self.lex_value.clone()
    }
}
impl Functional for TerminalNode {
    fn syntax_function(&self) -> SyntaxFunction {
        self.func
    }
}   

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SyntaxFunction {
    Program,
    StmtSeq,
    Stmt,
    ExprStmt,
    AssignmentStmt,
    IfStmt,
    ElseTail,
    VarDeclStmt,
    FunDefStmt,
    ParamSeq,
    ParamSeqTail,
    Param,
    ReturnDeclaration,
    ReturnStmt,
    TypeSpecifier,
    PrimitiveTypeSpecifier,
    ForLoopStmt,
    Let,
    For,
    In,
    Expr,
    Disjunction,
    ConjunctionSeq,
    Conjunction,
    InversionSeq,
    Inversion,
    Comparison,
    Sum,
    Term,
    Factor,
    CallExpr,
    Fn,
    ArgSeq,
    ArgSeqTail,
    Arg,
    Primary,
    Atom,
    RelOperator,
    MultOperator,
    AddOperator,
    Literal,
    BooleanLiteral,
    Semi,
    Coma,
    Return,
    RArrow,
    Identifier,
    Assign,
    If,
    Else,
    LBrace,
    RBrace,
    LParen,
    RParen,
    Or,
    And,
    Not,
    Less,
    Greater,
    LessEqual,
    GreaterEqual,
    Equal,
    NotEqual,
    Star,
    Div,
    Minus,
    Plus,
    IntegerLiteral,
    FloatingLiteral,
    StringLiteral,
    True,
    False,
    Colon,
    UnaryOperator,
    U32,
    I32,
    F32,
    U64,
    I64,
    F64,
    U8,
    Bool,
    StringType,
}
