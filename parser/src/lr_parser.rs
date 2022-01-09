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
                NonterminalNode::new("program", SyntaxFunction::Program)
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

                    parse_tree_node.reverse_children();

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

    fn nonterminal_node_from_grammar_symbol(&self, s: &Symbol) -> NonterminalNode {
        match s.id.as_str() {
            "program" => NonterminalNode::new("program", SyntaxFunction::Program),
            "stmtSeq" => NonterminalNode::new("stmtSeq", SyntaxFunction::StmtSeq),
            "stmt" => NonterminalNode::new("stmt", SyntaxFunction::Stmt),
            "exprStmt" => NonterminalNode::new("exprStmt", SyntaxFunction::ExprStmt),
            "assignmentStmt" => NonterminalNode::new("assignmentStmt", SyntaxFunction::AssignmentStmt),
            "ifStmt" => NonterminalNode::new("ifStmt", SyntaxFunction::IfStmt),
            "elseTail" => NonterminalNode::new("elseTail", SyntaxFunction::ElseTail),
            "expr" => NonterminalNode::new("expr", SyntaxFunction::Expr),
            "disjunction" => NonterminalNode::new("disjunction", SyntaxFunction::Disjunction),
            "conjunctionSeq" => NonterminalNode::new("conjunctionSeq", SyntaxFunction::ConjunctionSeq),
            "conjunction" => NonterminalNode::new("conjunction", SyntaxFunction::Conjunction),
            "inversionSeq" => NonterminalNode::new("inversionSeq", SyntaxFunction::InversionSeq),
            "inversion" => NonterminalNode::new("inversion", SyntaxFunction::Inversion),
            "comparison" => NonterminalNode::new("comparison", SyntaxFunction::Comparison),
            "sum" => NonterminalNode::new("sum", SyntaxFunction::Sum),
            "term" => NonterminalNode::new("term", SyntaxFunction::Term),
            "factor" => NonterminalNode::new("factor", SyntaxFunction::Factor),
            "primary" => NonterminalNode::new("primary", SyntaxFunction::Primary),
            "callExpr" => NonterminalNode::new("callExpr", SyntaxFunction::CallExpr),
            "argSeq" => NonterminalNode::new("argSeq", SyntaxFunction::ArgSeq),
            "arg" => NonterminalNode::new("arg", SyntaxFunction::Arg),
            "argSeqTail" => NonterminalNode::new("argSeqTail", SyntaxFunction::ArgSeqTail),
            "atom" => NonterminalNode::new("atom", SyntaxFunction::Atom),
            "relOperator" => NonterminalNode::new("relOperator", SyntaxFunction::RelOperator),
            "multOperator" => NonterminalNode::new("multOperator", SyntaxFunction::MultOperator),
            "addOperator" => NonterminalNode::new("addOperator", SyntaxFunction::AddOperator),
            "literal" => NonterminalNode::new("literal", SyntaxFunction::Literal),
            "booleanLiteral" => NonterminalNode::new("booleanLiteral", SyntaxFunction::BooleanLiteral),
            "varDeclStmt" => NonterminalNode::new("varDeclStmt", SyntaxFunction::VarDeclStmt),
            "forLoopStmt" => NonterminalNode::new("forLoopStmt", SyntaxFunction::ForLoopStmt),
            "funDefStmt" => NonterminalNode::new("funDefStmt", SyntaxFunction::FunDefStmt),
            "paramSeq" => NonterminalNode::new("paramSeq", SyntaxFunction::ParamSeq),
            "paramSeqTail" => NonterminalNode::new("paramSeqTail", SyntaxFunction::ParamSeqTail),
            "param" => NonterminalNode::new("param", SyntaxFunction::Param),
            "returnDeclaration" => NonterminalNode::new("returnDeclaration", SyntaxFunction::ReturnDeclaration),
            "returnStmt" => NonterminalNode::new("returnStmt", SyntaxFunction::ReturnStmt),
            "typeSpecifier" => NonterminalNode::new("typeSpecifier", SyntaxFunction::TypeSpecifier),
            "primitiveTypeSpecifier" => NonterminalNode::new("primitiveTypeSpecifier", SyntaxFunction::PrimitiveTypeSpecifier),
            "unaryOperator" => NonterminalNode::new("unaryOperator", SyntaxFunction::UnaryOperator),
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

    fn terminal_node_from_grammar_symbol(&self, s: &Symbol, lex_value: LexicalValue) -> TerminalNode {
        match s.id.as_str() {
            "Let" => TerminalNode::new("Let", lex_value, SyntaxFunction::Let),
            "For" => TerminalNode::new("For", lex_value, SyntaxFunction::For),
            "In" => TerminalNode::new("In", lex_value, SyntaxFunction::In),
            "Semi" => TerminalNode::new("Semi", lex_value, SyntaxFunction::Semi),
            "Identifier" => TerminalNode::new("Identifier", lex_value, SyntaxFunction::Identifier),
            "Assign" => TerminalNode::new("Assign", lex_value, SyntaxFunction::Assign),
            "If" => TerminalNode::new("If", lex_value, SyntaxFunction::If),
            "LBrace" => TerminalNode::new("LBrace", lex_value, SyntaxFunction::LBrace),
            "RBrace" => TerminalNode::new("RBrace", lex_value, SyntaxFunction::RBrace),
            "Or" => TerminalNode::new("Or", lex_value, SyntaxFunction::Or),
            "And" => TerminalNode::new("And", lex_value, SyntaxFunction::And),
            "Not" => TerminalNode::new("Not", lex_value, SyntaxFunction::Not),
            "Less" => TerminalNode::new("Less", lex_value, SyntaxFunction::Less),
            "Greater" => TerminalNode::new("Greater", lex_value, SyntaxFunction::Greater),
            "LessEqual" => TerminalNode::new("LessEqual", lex_value, SyntaxFunction::LessEqual),
            "GreaterEqual" => TerminalNode::new("GreaterEqual", lex_value, SyntaxFunction::GreaterEqual),
            "Equal" => TerminalNode::new("Equal", lex_value, SyntaxFunction::Equal),
            "NotEqual" => TerminalNode::new("NotEqual", lex_value, SyntaxFunction::NotEqual),
            "Star" => TerminalNode::new("Star", lex_value, SyntaxFunction::Star),
            "Div" => TerminalNode::new("Div", lex_value, SyntaxFunction::Div),
            "Minus" => TerminalNode::new("Minus", lex_value, SyntaxFunction::Minus),
            "Plus" => TerminalNode::new("Plus", lex_value, SyntaxFunction::Plus),
            "IntegerLiteral" => TerminalNode::new("IntegerLiteral", lex_value, SyntaxFunction::IntegerLiteral),
            "FloatingLiteral" => TerminalNode::new("FloatingLiteral", lex_value, SyntaxFunction::FloatingLiteral),
            "StringLiteral" => TerminalNode::new("StringLiteral", lex_value, SyntaxFunction::StringLiteral),
            "True" => TerminalNode::new("True", lex_value, SyntaxFunction::True),
            "False" => TerminalNode::new("False", lex_value, SyntaxFunction::False),
            "Else" => TerminalNode::new("Else", lex_value, SyntaxFunction::Else),
            "LParen" => TerminalNode::new("LParen", lex_value, SyntaxFunction::LParen),
            "RParen" => TerminalNode::new("RParen", lex_value, SyntaxFunction::RParen),
            "Coma" => TerminalNode::new("Coma", lex_value, SyntaxFunction::Coma),
            "Colon" => TerminalNode::new("Colon", lex_value, SyntaxFunction::Colon),
            "RArrow" => TerminalNode::new("RArrow", lex_value, SyntaxFunction::RArrow),
            "Fn" => TerminalNode::new("Fn", lex_value, SyntaxFunction::Fn),
            "Return" => TerminalNode::new("Return", lex_value, SyntaxFunction::Return),
            "U32" => TerminalNode::new("U32", lex_value, SyntaxFunction::U32),
            "I32" => TerminalNode::new("I32", lex_value, SyntaxFunction::I32),
            "F32" => TerminalNode::new("F32", lex_value, SyntaxFunction::F32),
            "U64" => TerminalNode::new("U64", lex_value, SyntaxFunction::U64),
            "I64" => TerminalNode::new("I64", lex_value, SyntaxFunction::I64),
            "F64" => TerminalNode::new("F64", lex_value, SyntaxFunction::F64),
            "U8" => TerminalNode::new("U8", lex_value, SyntaxFunction::U8),
            "Bool" => TerminalNode::new("Bool", lex_value, SyntaxFunction::Bool),
            "StringType" => TerminalNode::new("StringType", lex_value, SyntaxFunction::StringType),
            s => panic!("unknown terminal symbol {}", s),
        }
    }
}
