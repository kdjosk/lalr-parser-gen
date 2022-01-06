use std::any::Any;
use std::fmt::Display;
use lexer::Token;
use crate::grammar::{Production, Symbol};
use crate::lalr_parsing_tables::{Action, LALRParsingTables};
use crate::parse_tree::{self, *};

pub trait SymbolSource {
    fn next_symbol(&mut self) -> (Symbol, Token);
}

struct StackElement {
    state: usize,
    parse_tree_node: Box<dyn Node>,
}
impl StackElement {
    pub fn new(state: usize, parse_tree_node: Box<dyn Node>) -> StackElement {
        StackElement{
            state,
            parse_tree_node,
        }
    }

    pub fn starting() -> StackElement {
        StackElement {
            state: 0,
            parse_tree_node: Box::new(Nonterminal::new(Box::new(Program{}), vec![])),
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

    pub fn parse(&mut self) -> (Vec<Production>, Box<dyn Node>) {
        let (mut terminal, mut token) = self.symbol_source.next_symbol();
        loop {
            let action = self.parsing_tables.get_action(
                self.stack.last().unwrap().state,
                &terminal);
            match action {
                Action::Shift(state) => {
                    let lex_val = self.get_lexical_value_from_token(token);
                    let func = self.ast_operation_from_grammar_symbol(&terminal);
                    let leaf = Box::new(Terminal::new(func, lex_val));
                    self.stack.push(
                        StackElement::new(
                            state, 
                            leaf,
                        )
                    );
                    let temp = self.symbol_source.next_symbol();
                    terminal = temp.0;
                    token = temp.1;
                }
                Action::Reduce(p) => {
                    let rhs_len = p.rhs.len();
                    let mut parse_tree_node = Nonterminal::new(
                        self.ast_operation_from_grammar_symbol(&p.lhs),
                        vec![]);
                    for _ in 0..rhs_len {
                        let stack_el = self.stack.pop().unwrap();
                        parse_tree_node.add_child(stack_el.parse_tree_node);
                    }

                    let goto_state = self.parsing_tables.get_goto(
                        self.stack.last().unwrap().state, 
                        &p.lhs
                    );
                    self.stack.push(
                        StackElement::new(
                            goto_state,
                            Box::new(parse_tree_node),
                        )
                    );
                    self.output.push(p.clone());
                }
                Action::Accept => {
                    return (self.output.clone(), self.stack.pop().unwrap().parse_tree_node);
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

    fn get_lexical_value_from_token(&self, t: Token) -> Option<Box<dyn Display>> {
        match t {
            Token::IntegerLiteral(i) => Some(Box::new(i)),
            Token::StringLiteral(s) => Some(Box::new(s)),
            Token::FloatingLiteral(f) => Some(Box::new(f)),
            Token::Identifier(id) => Some(Box::new(id)),
            _ => None,
        }
    }

    fn ast_operation_from_grammar_symbol(&self, s: &Symbol) -> Box<dyn SyntaxFunction> {
        match s.id.as_str() {
            "program" => Box::new(Program{}),
            "stmtSeq" => Box::new(StmtSeq{}),
            "stmt" => Box::new(Stmt{}),
            "exprStmt" => Box::new(ExprStmt{}),
            "assignmentStmt" => Box::new(AssignmentStmt{}),
            "ifStmt" => Box::new(IfStmt{}),    
            "elseTail" => Box::new(ElseTail{}),
            "expr" => Box::new(Expr{}),
            "disjunction" => Box::new(Disjunction{}),
            "conjunctionSeq" => Box::new(ConjunctionSeq{}),
            "conjunction" => Box::new(Conjunction{}),
            "inversionSeq" => Box::new(InversionSeq{}),
            "inversion" => Box::new(Inversion{}),
            "comparison" => Box::new(Comparison{}),
            "sum" => Box::new(Sum{}),
            "term" => Box::new(Term{}),
            "factor" => Box::new(Factor{}),
            "primary" => Box::new(Primary{}),
            "callExpr" => Box::new(CallExpr{}),
            "argSeq" => Box::new(ArgSeq{}),
            "arg" => Box::new(Arg{}),
            "argSeqTail" => Box::new(ArgSeqTail{}),
            "atom" => Box::new(Atom{}),
            "relOperator" => Box::new(RelOperator{}),
            "multOperator" => Box::new(MultOperator{}),
            "addOperator" => Box::new(AddOperator{}),
            "literal" => Box::new(Literal{}),
            "booleanLiteral" => Box::new(BooleanLiteral{}),
            "varDeclStmt" => Box::new(VarDeclStmt{}),
            "forLoopStmt" => Box::new(ForLoopStmt{}),
            "Let" => Box::new(Let{}),
            "For" => Box::new(For{}),
            "In" => Box::new(In{}),
            "Semi" => Box::new(Semi{}),
            "Identifier" => Box::new(Identifier{}),
            "Assign" => Box::new(Assign{}),
            "If" => Box::new(If{}),
            "LBrace" => Box::new(LBrace{}),
            "RBrace" => Box::new(RBrace{}),
            "Or" => Box::new(Or{}),
            "And" => Box::new(And{}),
            "Not" => Box::new(Not{}),
            "Less" => Box::new(Less{}),
            "Greater" => Box::new(Greater{}),
            "LessEqual" => Box::new(LessEqual{}),
            "GreaterEqual" => Box::new(GreaterEqual{}),
            "Equal" => Box::new(Equal{}),
            "NotEqual" => Box::new(NotEqual{}),
            "Star" => Box::new(Star{}),
            "Div" => Box::new(Div{}),
            "Minus" => Box::new(Minus{}),
            "Plus" => Box::new(Plus{}),
            "IntegerLiteral" => Box::new(IntegerLiteral{}),
            "FloatingLiteral" => Box::new(FloatingLiteral{}),
            "StringLiteral" => Box::new(StringLiteral{}),
            "True" => Box::new(True{}),
            "False" => Box::new(False{}),
            "Else" => Box::new(Else{}),
            "LParen" => Box::new(LParen{}),
            "RParen" => Box::new(RParen{}),
            "Coma" => Box::new(Coma{}),
            "funDefStmt" => Box::new(FunDefStmt{}),
            "paramSeq" => Box::new(ParamSeq{}),
            "paramSeqTail" => Box::new(ParamSeqTail{}),
            "param" => Box::new(Param{}),
            "returnDeclaration" => Box::new(ReturnDeclaration{}),
            "typeSpecifier" => Box::new(TypeSpecifier{}),
            "primitiveTypeSpecifier" => Box::new(PrimitiveTypeSpecifier{}),
            "unaryOperator" => Box::new(UnaryOperator{}),
            "Colon" => Box::new(Colon{}),
            "RightArrow" => Box::new(RightArrow{}),
            "Fn" => Box::new(parse_tree::Fn{}),
            "U32" => Box::new(U32{}),
            "I32" => Box::new(I32{}),
            "F32" => Box::new(F32{}),
            "U64" => Box::new(U64{}),
            "I64" => Box::new(I64{}),
            "F64" => Box::new(F64{}),
            "U8" => Box::new(U8{}),
            "Bool" => Box::new(Bool{}),
            "StringType" => Box::new(StringType{}),
            s => panic!("unknown symbol {}", s),
        }
    }
}

