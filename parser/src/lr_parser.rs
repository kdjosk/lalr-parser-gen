use crate::grammar::{Production, Symbol};
use crate::lalr_parsing_tables::{Action, LALRParsingTables};
use crate::parse_tree::*;
use lexer::Token;
use std::any::Any;
use std::fmt::Display;

pub trait SymbolSource {
    fn next_symbol(&mut self) -> (Symbol, Token);
}

struct StackElement {
    state: usize,
    parse_tree_node: ParseTreeNode,
}

impl StackElement {
    pub fn new(state: usize, parse_tree_node: ParseTreeNode) -> StackElement {
        StackElement {
            state,
            parse_tree_node,
        }
    }

    pub fn starting() -> StackElement {
        StackElement {
            state: 0,
            parse_tree_node: ParseTreeNode::Internal(
                Box::new(NonterminalNode::new("program", Program{}))
            ),
        }
    }
}

pub struct LRParser<T: SymbolSource> {
    parsing_tables: LALRParsingTables,
    stack: Vec<StackElement>,
    output: Vec<Production>,
    symbol_source: T,
}
impl<T: SymbolSource> LRParser<T> {
    pub fn new(parsing_tables: LALRParsingTables, symbol_source: T) -> LRParser<T> {
        LRParser {
            parsing_tables,
            stack: vec![StackElement::starting()],
            output: vec![],
            symbol_source,
        }
    }

    pub fn parse(&mut self) -> (Vec<Production>, ParseTreeNode) {
        let (mut terminal, mut token) = self.symbol_source.next_symbol();
        loop {
            let action = self
                .parsing_tables
                .get_action(self.stack.last().unwrap().state, &terminal);
            match action {
                Action::Shift(state) => {
                    let lex_value = self.lexical_value_from_token(token);
                    let leaf = self.terminal_node_from_grammar_symbol(&terminal, lex_value);
                    let leaf = ParseTreeNode::Leaf(leaf);
                    self.stack.push(StackElement::new(state, leaf));
                    let temp = self.symbol_source.next_symbol();
                    terminal = temp.0;
                    token = temp.1;
                }
                Action::Reduce(p) => {
                    let rhs_len = p.rhs.len();
                    let mut parse_tree_node = self.nonterminal_node_from_grammar_symbol(&p.lhs);
                    for _ in 0..rhs_len {
                        let stack_el = self.stack.pop().unwrap();
                        parse_tree_node.add_child(stack_el.parse_tree_node);
                    }

                    let goto_state = self
                        .parsing_tables
                        .get_goto(self.stack.last().unwrap().state, &p.lhs);
                    self.stack
                        .push(StackElement::new(
                            goto_state, 
                            ParseTreeNode::Internal(parse_tree_node))
                        );
                    self.output.push(p.clone());
                }
                Action::Accept => {
                    let node = self.stack.pop().unwrap().parse_tree_node;
                    return (
                        self.output.clone(),
                        node,
                    );
                }
                Action::Error => {
                    println!("STACK: ");
                    for s in &self.stack {
                        print!("{}, ", s.state);
                    }
                    panic!("ERROR AT SYMBOL {}", terminal);
                }
            }
        }
    }

    fn nonterminal_node_from_grammar_symbol(&self, s: &Symbol) -> Box<dyn NonterminalNodeT> {
        match s.id.as_str() {
            "program" => Box::new(NonterminalNode::new("program", Program {})),
            "stmtSeq" => Box::new(NonterminalNode::new("stmtSeq", StmtSeq {})),
            "stmt" => Box::new(NonterminalNode::new("stmt", Stmt {})),
            "exprStmt" => Box::new(NonterminalNode::new("exprStmt", ExprStmt {})),
            "assignmentStmt" => Box::new(NonterminalNode::new("assignmentStmt", AssignmentStmt {})),
            "ifStmt" => Box::new(NonterminalNode::new("ifStmt", IfStmt {})),
            "elseTail" => Box::new(NonterminalNode::new("elseTail", ElseTail {})),
            "expr" => Box::new(NonterminalNode::new("expr", Expr {})),
            "disjunction" => Box::new(NonterminalNode::new("disjunction", Disjunction {})),
            "conjunctionSeq" => Box::new(NonterminalNode::new("conjunctionSeq", ConjunctionSeq {})),
            "conjunction" => Box::new(NonterminalNode::new("conjunction", Conjunction {})),
            "inversionSeq" => Box::new(NonterminalNode::new("inversionSeq", InversionSeq {})),
            "inversion" => Box::new(NonterminalNode::new("inversion", Inversion {})),
            "comparison" => Box::new(NonterminalNode::new("comparison", Comparison {})),
            "sum" => Box::new(NonterminalNode::new("sum", Sum {})),
            "term" => Box::new(NonterminalNode::new("term", Term {})),
            "factor" => Box::new(NonterminalNode::new("factor", Factor {})),
            "primary" => Box::new(NonterminalNode::new("primary", Primary {})),
            "callExpr" => Box::new(NonterminalNode::new("callExpr", CallExpr {})),
            "argSeq" => Box::new(NonterminalNode::new("argSeq", ArgSeq {})),
            "arg" => Box::new(NonterminalNode::new("arg", Arg {})),
            "argSeqTail" => Box::new(NonterminalNode::new("argSeqTail", ArgSeqTail {})),
            "atom" => Box::new(NonterminalNode::new("atom", Atom {})),
            "relOperator" => Box::new(NonterminalNode::new("relOperator", RelOperator {})),
            "multOperator" => Box::new(NonterminalNode::new("multOperator", MultOperator {})),
            "addOperator" => Box::new(NonterminalNode::new("addOperator", AddOperator {})),
            "literal" => Box::new(NonterminalNode::new("literal", Literal {})),
            "booleanLiteral" => Box::new(NonterminalNode::new("booleanLiteral", BooleanLiteral {})),
            "varDeclStmt" => Box::new(NonterminalNode::new("varDeclStmt", VarDeclStmt {})),
            "forLoopStmt" => Box::new(NonterminalNode::new("forLoopStmt", ForLoopStmt {})),
            "funDefStmt" => Box::new(NonterminalNode::new("funDefStmt", FunDefStmt {})),
            "paramSeq" => Box::new(NonterminalNode::new("paramSeq", ParamSeq {})),
            "paramSeqTail" => Box::new(NonterminalNode::new("paramSeqTail", ParamSeqTail {})),
            "param" => Box::new(NonterminalNode::new("param", Param {})),
            "returnDeclaration" => Box::new(NonterminalNode::new("returnDeclaration", ReturnDeclaration {})),
            "returnStmt" => Box::new(NonterminalNode::new("returnStmt", ReturnStmt {})),
            "typeSpecifier" => Box::new(NonterminalNode::new("typeSpecifier", TypeSpecifier {})),
            "primitiveTypeSpecifier" => Box::new(NonterminalNode::new("primitiveTypeSpecifier", PrimitiveTypeSpecifier {})),
            "unaryOperator" => Box::new(NonterminalNode::new("unaryOperator", UnaryOperator {})),
            s => panic!("Unknown nonterminal symbol {}", s),
        }
    }

    fn lexical_value_from_token(&self, t: Token) -> LexicalValue {
        match t {
            Token::IntegerLiteral(i) => LexicalValue::IntegerType(i),
            Token::StringLiteral(s) => LexicalValue::StringType(s),
            Token::FloatingLiteral(f) => LexicalValue::FloatingType(f),
            Token::Identifier(id) => LexicalValue::IdentifierType(id),
            _ => LexicalValue::NoLexicalValue,
        }
    }

    fn terminal_node_from_grammar_symbol(&self, s: &Symbol, lex_value: LexicalValue) -> Box<dyn TerminalNodeT> {
        match s.id.as_str() {
            "Let" => Box::new(TerminalNode::new("Let", lex_value, Let {})),
            "For" => Box::new(TerminalNode::new("For", lex_value, For {})),
            "In" => Box::new(TerminalNode::new("In", lex_value, In {})),
            "Semi" => Box::new(TerminalNode::new("Semi", lex_value, Semi {})),
            "Identifier" => Box::new(TerminalNode::new("Identifier", lex_value, Identifier {})),
            "Assign" => Box::new(TerminalNode::new("Assign", lex_value, Assign {})),
            "If" => Box::new(TerminalNode::new("If", lex_value, If {})),
            "LBrace" => Box::new(TerminalNode::new("LBrace", lex_value, LBrace {})),
            "RBrace" => Box::new(TerminalNode::new("RBrace", lex_value, RBrace {})),
            "Or" => Box::new(TerminalNode::new("Or", lex_value, Or {})),
            "And" => Box::new(TerminalNode::new("And", lex_value, And {})),
            "Not" => Box::new(TerminalNode::new("Not", lex_value, Not {})),
            "Less" => Box::new(TerminalNode::new("Less", lex_value, Less {})),
            "Greater" => Box::new(TerminalNode::new("Greater", lex_value, Greater {})),
            "LessEqual" => Box::new(TerminalNode::new("LessEqual", lex_value, LessEqual {})),
            "GreaterEqual" => Box::new(TerminalNode::new("GreaterEqual", lex_value, GreaterEqual {})),
            "Equal" => Box::new(TerminalNode::new("Equal", lex_value, Equal {})),
            "NotEqual" => Box::new(TerminalNode::new("NotEqual", lex_value, NotEqual {})),
            "Star" => Box::new(TerminalNode::new("Star", lex_value, Star {})),
            "Div" => Box::new(TerminalNode::new("Div", lex_value, Div {})),
            "Minus" => Box::new(TerminalNode::new("Minus", lex_value, Minus {})),
            "Plus" => Box::new(TerminalNode::new("Plus", lex_value, Plus {})),
            "IntegerLiteral" => Box::new(TerminalNode::new("IntegerLiteral", lex_value, IntegerLiteral {})),
            "FloatingLiteral" => Box::new(TerminalNode::new("FloatingLiteral", lex_value, FloatingLiteral {})),
            "StringLiteral" => Box::new(TerminalNode::new("StringLiteral", lex_value, StringLiteral {})),
            "True" => Box::new(TerminalNode::new("True", lex_value, True {})),
            "False" => Box::new(TerminalNode::new("False", lex_value, False {})),
            "Else" => Box::new(TerminalNode::new("Else", lex_value, Else {})),
            "LParen" => Box::new(TerminalNode::new("LParen", lex_value, LParen {})),
            "RParen" => Box::new(TerminalNode::new("RParen", lex_value, RParen {})),
            "Coma" => Box::new(TerminalNode::new("Coma", lex_value, Coma {})),
            "Colon" => Box::new(TerminalNode::new("Colon", lex_value, Colon {})),
            "RArrow" => Box::new(TerminalNode::new("RArrow", lex_value, RArrow {})),
            "Fun" => Box::new(TerminalNode::new("Fun", lex_value, Fun {})),
            "Return" => Box::new(TerminalNode::new("Return", lex_value, Return {})),
            "U32" => Box::new(TerminalNode::new("U32", lex_value, U32 {})),
            "I32" => Box::new(TerminalNode::new("I32", lex_value, I32 {})),
            "F32" => Box::new(TerminalNode::new("F32", lex_value, F32 {})),
            "U64" => Box::new(TerminalNode::new("U64", lex_value, U64 {})),
            "I64" => Box::new(TerminalNode::new("I64", lex_value, I64 {})),
            "F64" => Box::new(TerminalNode::new("F64", lex_value, F64 {})),
            "U8" => Box::new(TerminalNode::new("U8", lex_value, U8 {})),
            "Bool" => Box::new(TerminalNode::new("Bool", lex_value, Bool {})),
            "StringType" => Box::new(TerminalNode::new("StringType", lex_value, StringType {})),
            s => panic!("unknown terminal symbol {}", s),
        }
    }
}
